;;; url-http-ntlm.el --- NTLM authentication for the url library

;; Copyright (C) 2008, 2015 Free Software Foundation, Inc.

;; Author: Tom Schutzer-Weissmann <tom.weissmann@gmail.com>
;; Maintainer: Thomas Fitzsimmons <fitzsim@fitzsim.org>
;; Keywords: comm, data, processes, hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a NTLM handler for the URL package.
;;
;; Installation:
;;
;; M-x package-install RET url-http-ntlm RET
;;
;; Acknowledgements:
;;
;; Taro Kawagishi <tarok@transpulse.org> wrote ntlm.el and md4.el,
;; which are parts of FLIM (Faithful Library about Internet Message).
;;
;; http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/flim-1.14.7/ntlm.el
;; http://stuff.mit.edu/afs/sipb/contrib/emacs/packages/flim-1.14.7/md4.el

;;; Code:
(require 'url-auth)
(require 'url-http)
(require 'mail-parse)
(require 'cl-lib)
(require 'ntlm)

;; Remove authorization after redirect.
(when (and (boundp 'emacs-major-version)
	   (< emacs-major-version 25))
  (require (intern (format "url-http-ntlm-parse-headers-%d.%d"
			   emacs-major-version
			   emacs-minor-version))))


;;; Private variables.
(defvar url-http-ntlm--auth-storage nil
  "Authentication storage.
An alist that maps a server name to a pair of \(<username> <ntlm
hashes>\).

The hashes are built using `ntlm-get-password-hashes'.")

(defvar url-http-ntlm--last-args nil
  "Stores the last `url-http-ntlm--get-stage' arguments and return value.
This is used to detect multiple calls.")
(make-variable-buffer-local 'url-http-ntlm--last-args)

(defvar url-http-ntlm--loop-timer-counter nil
  "A hash table used to detect NTLM negotiation errors.
Keys are urls, entries are (START-TIME . COUNTER).")

(defvar url-http-ntlm--default-users nil
  "An alist that maps each server to the default username for
that server.")


;;; Private functions.
(defun url-http-ntlm--detect-loop (url)
  "Detect potential infinite loop when NTLM fails on URL."
  (when (not url-http-ntlm--loop-timer-counter)
    (setq url-http-ntlm--loop-timer-counter (make-hash-table :test 'equal)))
  (let* ((url-string (url-recreate-url url))
	 (last-entry (gethash url-string url-http-ntlm--loop-timer-counter))
	 (start-time (car last-entry))
	 (counter (cdr last-entry)))
    (if last-entry
	(progn
	  (if (< (-  (float-time) start-time) 10.0)
	      (if (< counter 20)
		  ;; Still within time window, so increment count.
		  (puthash url-string (cons start-time (1+ counter))
			   url-http-ntlm--loop-timer-counter)
		;; Error detected, so remove entry and clear.
		(url-http-ntlm--authorisation url-string :clear)
		(remhash url-string url-http-ntlm--loop-timer-counter)
		(error
		 (format (concat "Access rate to %s is too high,"
				 " indicating an NTLM failure;"
				 " to debug, re-run with url-debug set to 1")
			 url-string)))
	    ;; Timeout expired, so reset counter.
	    (puthash url-string (cons (float-time) 0)
		     url-http-ntlm--loop-timer-counter)))
      ;; New access, so initialize counter to 0.
      (puthash url-string (cons (float-time) 0)
	       url-http-ntlm--loop-timer-counter))))

(defun url-http-ntlm--ensure-user (url)
  "Return URL with its user slot set.
If URL's user slot is nil, set it to the last user that made a
request to the host in URL's server slot."
  (let ((new-url url))
    (if (url-user new-url)
	new-url
      (setf (url-user new-url)
	    (cdr (assoc (url-host new-url) url-http-ntlm--default-users)))
      new-url)))

(defun url-http-ntlm--ensure-keepalive ()
  "Report an error if `url-http-attempt-keepalives' is not set."
  (cl-assert url-http-attempt-keepalives
	     nil
	     (concat "NTLM authentication won't work unless"
		     " `url-http-attempt-keepalives' is set!")))

(defun url-http-ntlm--clean-headers ()
  "Remove Authorization element from `url-http-extra-headers' alist."
  (cl-declare (special url-http-extra-headers))
  (setq url-http-extra-headers
	(url-http-ntlm--rmssoc "Authorization" url-http-extra-headers)))

(defun url-http-ntlm--get-stage (args)
  "Determine what stage of the NTLM handshake we are at.
PROMPT and ARGS come from `url-ntlm-auth''s caller,
`url-get-authentication'.  Their meaning depends on the current
implementation - this function is well and truly coupled.

url-get-authentication' calls `url-ntlm-auth' once when checking
what authentication schemes are supported (PROMPT and ARGS are
nil), and then twice for every stage of the handshake: the first
time PROMPT is nil, the second, t; ARGS contains the server
response's \"WWW-Authenticate\" header, munged by
`url-parse-args'."
  (cl-declare (special url-http-extra-headers))
  (let* ((response-rxp	   "^NTLM TlRMTVNTUAADAAA")
	 (challenge-rxp	   "^TLRMTVNTUAACAAA")
	 (auth-header	   (assoc "Authorization" url-http-extra-headers))
	 (case-fold-search t)
	 stage)
    (if (eq args (car url-http-ntlm--last-args))
	;; multiple calls, return the same argument we returned last time
	(cdr url-http-ntlm--last-args)
      (let ((stage
	     (cond ((and auth-header (string-match response-rxp
						   (cdr auth-header)))
		    :error)
		   ((and (= (length args) 2)
			 (cl-destructuring-bind (challenge ntlm) args
			   (and (string-equal "ntlm" (car ntlm))
				(string-match challenge-rxp
					      (car challenge)))))
		    :response)
		   (t
		    :request))))
	(url-http-ntlm--clean-headers)
	(setq url-http-ntlm--last-args (cons args stage))
	stage))))

(defun url-http-ntlm--authorisation (url &optional clear realm)
  "Get or clear NTLM authentication details for URL.
If CLEAR is non-nil, clear any saved credentials for server.
Otherwise, return the credentials, prompting the user if
necessary.  REALM appears in the prompt.

If URL contains a username and a password, they are used and
stored credentials are not affected."
  (let* ((href   (if (stringp url)
		     (url-generic-parse-url url)
		   url))
	 (type   (url-type href))
	 (user   (url-user href))
	 (server (url-host href))
	 (port   (url-portspec href))
	 (pass   (url-password href))
	 (stored (assoc (list type user server port)
			url-http-ntlm--auth-storage))
	 (both   (and user pass)))
    (if clear
	;; clear
	(unless both
	  (setq url-http-ntlm--default-users
		(url-http-ntlm--rmssoc server url-http-ntlm--default-users))
	  (setq url-http-ntlm--auth-storage
		(url-http-ntlm--rmssoc '(type user* server port)
				       url-http-ntlm--auth-storage))
	  nil)
      ;; get
      (if (or both
	      (and stored user (not (equal user (cl-second (car stored)))))
	      (not stored))
	  (let* ((user* (or user
			    (url-do-auth-source-search server type :user)
			    (read-string (url-auth-user-prompt url realm)
					 (or user (user-real-login-name)))))
		 (pass* (if both
			    pass
			  (or (url-do-auth-source-search server type :secret)
			      (read-passwd (format "Password [for %s]: "
						   (url-recreate-url url))))))
		 (key   (list type user* server port))
		 (entry `(,key . (,(ntlm-get-password-hashes pass*)))))
	    (unless both
	      (setq url-http-ntlm--default-users
		    (cons
		     `(,server . ,user*)
		     (url-http-ntlm--rmssoc server
					    url-http-ntlm--default-users)))
	      (setq url-http-ntlm--auth-storage
		    (cons entry
			  (url-http-ntlm--rmssoc
			   key
			   url-http-ntlm--auth-storage))))
	    entry)
	stored))))

(defun url-http-ntlm--get-challenge ()
  "Return the NTLM Type-2 message in the WWW-Authenticate header, if present."
  (save-restriction
    (mail-narrow-to-head)
    (let ((www-authenticate (mail-fetch-field "www-authenticate")))
      (when (string-match "NTLM\\s-+\\(\\S-+\\)"
			  www-authenticate)
	(base64-decode-string (match-string 1 www-authenticate))))))

(defun url-http-ntlm--rmssoc (key alist)
  "Remove all elements whose `car' match KEY from ALIST."
  (cl-remove key alist :key 'car :test 'equal))

(defun url-http-ntlm--string (data)
  "Return DATA encoded as an NTLM string."
  (concat "NTLM " (base64-encode-string data :nobreak)))


;;; Public function called by `url-get-authentication'.
;;;###autoload
(defun url-ntlm-auth (url &optional prompt overwrite realm args)
  "Return an NTLM HTTP authorization header.
Get the contents of the Authorization header for a HTTP response
using NTLM authentication, to access URL.  Because NTLM is a
two-step process, this function expects to be called twice, first
to generate the NTLM type 1 message (request), then to respond to
the server's type 2 message (challenge) with a suitable response.

PROMPT, OVERWRITE, and REALM are ignored.

ARGS is expected to contain the WWW-Authentication header from
the server's last response.  These are used by
`url-http-get-stage' to determine what stage we are at."
  (url-http-ntlm--ensure-keepalive)
  (let* ((user-url (url-http-ntlm--ensure-user url))
	 (stage (url-http-ntlm--get-stage args)))
    (cl-case stage
      ;; NTLM Type 1 message: the request
      (:request
       (url-http-ntlm--detect-loop user-url)
       (cl-destructuring-bind (&optional key hash)
	   (url-http-ntlm--authorisation user-url nil realm)
	 (when (cl-third key)
	   (url-http-ntlm--string
	    (ntlm-build-auth-request (cl-second key) (cl-third key))))))
      ;; NTLM Type 3 message: the response
      (:response
       (url-http-ntlm--detect-loop user-url)
       (let ((challenge (url-http-ntlm--get-challenge)))
	 (cl-destructuring-bind (key hash)
	     (url-http-ntlm--authorisation user-url nil realm)
	   (url-http-ntlm--string
	    (ntlm-build-auth-response challenge
				      (cl-second key)
				      hash)))))
      (:error
       (url-http-ntlm--authorisation user-url :clear)))))


;;; Register `url-ntlm-auth' HTTP authentication method.
;;;###autoload
(url-register-auth-scheme "ntlm" nil 8)

(provide 'url-http-ntlm)

;;; url-http-ntlm.el ends here
