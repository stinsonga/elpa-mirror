;;; gnus-mock.el --- Mock Gnus installation for testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Package-Type: multi
;; Version: 0.4.5

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides a reproducible Gnus installation, including
;; dummy data, that can be used for Gnus development and testing.
;; Call `gnus-mock-start' from your currently-running Emacs to start a
;; new Emacs instance, skipping all user init (ie startup as -Q), but
;; preloading a mock Gnus installation.  All normal Gnus startup
;; commands will begin a session within this mock installation.

;; The developer can also specify a different Emacs executable to
;; start (for instance, when working on a Git branch checked out in a
;; worktree).  This is controlled by the `gnus-mock-emacs-program'
;; option.

;; The mock session starts with some predefined servers, as well as
;; some dummy mail data.  At startup, all dummy data is copied into a
;; temporary directory, which is deleted at shutdown.  The environment
;; can thus be loaded, tweaked, trashed, and re-loaded with impunity.
;; To fully restore a clean testing environment, simply quit the Emacs
;; process and restart it from the parent process by running
;; `gnus-mock-start' again.  Alternately it's possible to restart "in
;; place" by calling `gnus-mock-reload', though, depending on what the
;; developer has gotten up to, this isn't guaranteed to completely
;; restore the environment.

;; Users have two options for adding custom configuration to the mock
;; session:

;; - `gnus-mock-gnus-file' can be set to a filename, the contents
;;    of which will be appended to the .gnus.el startup file in the
;;    mock session.  This code will be executed at Gnus startup.

;; - `gnus-mock-init-file' should also be a filename, the contents
;;   of which will be appended to the init.el file that is loaded when
;;   the child Emacs process starts.

;; It's possible to compose and send mail in a mock Gnus session; the
;; mail will be sent using the value of `gnus-mock-sendmail-program'.
;; If Python is available on the user's system, this option will be
;; set to a Python program that simply accepts the outgoing mail and
;; shunts it to the "incoming" mailbox of the pre-defined nnmaildir
;; server.

;;; Code:

(require 'gnus)
(require 'message)

(defgroup gnus-mock nil
  "Options for the mock Gnus installation."
  :group 'gnus)

(defcustom gnus-mock-gnus-file nil
  "Path to an additional Gnus config file for mock Gnus.
The contents of this file will be appended to gnus-mock's Gnus
init file, which will be loaded when Gnus is started."
  :type 'file)

(defcustom gnus-mock-init-file nil
  "Path to an additional init config file for mock Gnus.
The contents of this file will be appended to gnus-mock's init
file, which will be loaded when the child Emacs process is
started."
  :type 'file)

(defcustom gnus-mock-emacs-program "emacs"
  "Name of the Emacs executable to use for the mock session."
  :type 'string)

(defcustom gnus-mock-cleanup-p t
  "When non-nil, delete temporary files after shutdown.
Each Gnus mock session will create a unique temporary directory,
so multiple sessions will not conflict if this option is nil."
  :type 'boolean)

(defcustom gnus-mock-use-images t
  "When non-nil, use some cute Gnus-mock-specific images."
  :type 'boolean)

(defcustom gnus-mock-sendmail-program
  (when (executable-find "python")
    (if (memq system-type '(cygwin ms-dos windows-nt))
	"windows-sendmail-wrapper.cmd"
      "fakesendmail.py"))
  "Program used as the value of `sendmail-program'."
  :type 'string)

(defcustom gnus-mock-dovecot-imap-program nil
  "Dovecot imap executable used to set up an nnimap server.
This option should not be set to the \"dovecot\" executable
itself, but rather to the \"imap\" executable that ships with
dovecot.  This executable isn't on the PATH, but often lives at
/usr/lib/dovecot/imap.

If nil, no nnimap server will be added to the Gnus mock
installation."
  :type 'string)

(defconst gnus-mock-data-dir
  (file-name-as-directory (expand-file-name
			   "data"
			   (if load-file-name
			       (file-name-directory load-file-name)
			     default-directory)))
  "Source directory for Gnus mock data.")

;;;###autoload
(defun gnus-mock-start ()
  "Start a new Emacs process, with the Gnus mock setup.
The new Emacs process will be started as \"-Q\", with the mock
Gnus settings pre-loaded.  Any of the normal Gnus entry points
will start a mock Gnus session."
  (interactive)
  (let ((mock-tmp-dir (make-temp-file "emacs-gnus-mock-" t)))
    (condition-case-unless-debug err
	(let ((init-file (expand-file-name "init.el" mock-tmp-dir))
	      (maildir-sep (if (memq system-type '(ms-dos windows-nt))
			       "-" ":"))
	      (maildir-folders
	       '("test/Welcome" "test/incoming" "test/mails"
		 "imapmail/mail" "imapmail/mail/.emacs-devel"
		 "imapmail/mail/.&BB8EQAQ4BDIENQRCBDwEOARA-")))
	  (with-temp-buffer
	    (let ((standard-output (current-buffer))
		  (print-circle nil))
	      (prin1
	       `(setq gnus-home-directory ,mock-tmp-dir
		      init-file-user "mockturtle"
		      sendmail-program
		      ,(expand-file-name gnus-mock-sendmail-program
					 mock-tmp-dir)
		      message-directory ,mock-tmp-dir
		      gnus-startup-file
		      ,(expand-file-name ".newsrc" mock-tmp-dir)
		      gnus-init-file
		      ,(expand-file-name ".gnus.el" mock-tmp-dir)
		      nndraft-directory
		      ,(expand-file-name "drafts/" mock-tmp-dir)
		      gnus-agent-directory
		      ,(expand-file-name "agent/" mock-tmp-dir)
		      gnus-directory
		      ,(expand-file-name "News/" mock-tmp-dir)))
	      (princ "\n\n")
	      ;; Constant that can be checked if we need to know it's a mock
	      ;; session.
	      (prin1 '(defconst gnus-mock-p t))
	      (princ "\n\n")
	      ;; Load our interactive testing file.  Does package.el
	      ;; compile these files by default?
	      (prin1 `(require
		       'gnus-mock-tests
		       ,(expand-file-name "gnus-mock-tests" mock-tmp-dir)))
	      (princ "\n\n")
	      ;; Constant for use in `gnus-mock-reload', which is defined in
	      ;; the .gnus.el startup file.
 	      (prin1 `(defconst gnus-mock-data-dir ,gnus-mock-data-dir))
	      (when gnus-mock-cleanup-p
		(princ "\n")
		(prin1 `(add-hook 'kill-emacs-hook
				  (lambda () (delete-directory
					      ,mock-tmp-dir t)))))
	      (when gnus-mock-use-images
		(princ "\n")
		(prin1 `(add-to-list 'load-path
				     ,(format "%s/data" mock-tmp-dir))))
	      (write-file init-file)))
	  ;; Put our data and config in place.
	  (copy-directory
	   gnus-mock-data-dir
	   (file-name-as-directory mock-tmp-dir) nil nil t)
	  ;; Git doesn't let us commit empty directories, so create
	  ;; our necessary empty maildir bits, and draft directories.
	  ;; Some of these directories already exist; `make-directory'
	  ;; with the PARENTS arg will silently pass over them.
	  (mapc (lambda (path)
		  (mapc
		   (lambda (bit)
		     (make-directory
		      (expand-file-name bit path) t))
		   '("new" "tmp" "cur")))
		(mapcar (lambda (dir)
			  (expand-file-name dir mock-tmp-dir))
			maildir-folders))
	  (mapc (lambda (dir)
		  (make-directory
		   (expand-file-name dir mock-tmp-dir)
		   t))
		'("test/Welcome/.nnmaildir/marks"
		  "test/incoming/.nnmaildir/marks"
		  "test/incoming/.nnmaildir/nov"
		  "test/mails/.nnmaildir/marks"))
	  (make-directory (format "%s/drafts/drafts" mock-tmp-dir) t)
	  (make-directory (format "%s/drafts/queue" mock-tmp-dir))
	  ;; Rename all our maildir message files with a separator
	  ;; character that doesn't offend the filesystem.
	  (dolist (dir maildir-folders)
	    (dolist (file (directory-files
			   (expand-file-name
			    "cur"
			    (expand-file-name dir mock-tmp-dir))
			   t "SEP"))
	      (rename-file
	       file (replace-regexp-in-string "SEP" maildir-sep file))))
	  ;; Possibly insert additional config.
	  (when gnus-mock-init-file
	    (with-temp-buffer
	      (insert-file-contents gnus-mock-init-file)
	      (append-to-file
	       (point-min) (point-max) init-file)))
	  (when gnus-mock-gnus-file
	    (with-temp-buffer
	      (insert-file-contents gnus-mock-gnus-file)
	      (append-to-file
	       (point-min) (point-max)
	       (expand-file-name ".gnus.el" mock-tmp-dir))))
	  ;; Possibly add an nnimap server.
	  (when gnus-mock-dovecot-imap-program
	    (with-temp-buffer
	      (insert "\n\n")
	      (prin1
	       `(add-to-list
		 'gnus-secondary-select-methods
		 (quote (nnimap
			 "Mocky"
			 (nnimap-stream shell)
			 (nnimap-shell-program
			  ,(concat
			    gnus-mock-dovecot-imap-program
			    (format " -o mail_location=maildir:%s/imapmail/mail"
				    mock-tmp-dir))))))
	       (current-buffer))
	      (append-to-file
	       (point-min) (point-max)
	       (expand-file-name ".gnus.el" mock-tmp-dir))))
	  ;; There are absolute paths in the .newsrc.eld file, so doctor
	  ;; that file.
	  (with-current-buffer (find-file-noselect
				(expand-file-name ".newsrc.eld" mock-tmp-dir))
	    (while (re-search-forward "REPLACE_ME" (point-max) t)
	      (replace-match mock-tmp-dir t))
	    (when gnus-mock-dovecot-imap-program
	      (goto-char (point-max))
	      (insert "\n\n")
	      (prin1
	       '(setq
		 gnus-newsrc-alist
		 (append
		  gnus-newsrc-alist
		  '(("nnimap+Mocky:INBOX" 3 nil ((unexist) (seen (1 . 32)))
		     "nnimap:Mocky" ((modseq . "33") (uidvalidity . "1541087103")
				     (active 1 . 32)
				     (permanent-flags %Answered %Flagged %Deleted
						      %Seen %Draft %*)))
		    ("nnimap+Mocky:emacs-devel" 3 nil ((unexist 0))
		     "nnimap:Mocky" ((modseq . "21") (uidvalidity . "1541087104")
				     (active 0 . 20)
				     (permanent-flags %Answered %Flagged %Deleted
						      %Seen %Draft %*)))
		    ("nnimap+Mocky:\320\237\321\200\320\270\320\262\320\265\321\202\320\274\320\270\321\200"
		     3 nil ((unexist) (seen 1))
		     "nnimap:Mocky" ((modseq . "2") (uidvalidity . "1541087105")
				     (active 1 . 1)
				     (permanent-flags %Answered %Flagged %Deleted
						      %Seen %Draft %*))))))
	       (current-buffer)))
	    (basic-save-buffer))
	  (let ((default-directory
		  (expand-file-name
		   "lisp/gnus"
		   (file-name-as-directory
		    (expand-file-name "../.."
				      gnus-mock-emacs-program)))))
	    (make-process :name "gnus-mock" :buffer nil
			  :command (list gnus-mock-emacs-program
					 "-Q" "--load" init-file)
			  :stderr "*gnus mock errors*")))
      (error (when (and gnus-mock-cleanup-p
			(file-exists-p mock-tmp-dir))
	       (delete-directory mock-tmp-dir t))
	     (signal (car err) (cdr err))))))

(provide 'gnus-mock)
;;; gnus-mock.el ends here
