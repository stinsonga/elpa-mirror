;;; debbugs-gnu.el --- interface for the GNU bug tracker

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: comm, hypermedia, maint
;; Package: debbugs
;; Version: 0.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'debbugs)
(eval-when-compile (require 'cl))

(autoload 'gnus-read-ephemeral-emacs-bug-group "gnus-group")
(autoload 'mail-header-subject "nnheader")
(autoload 'gnus-summary-article-header "gnus-sum")
(autoload 'message-make-from "message")

(defface debbugs-new '((t (:foreground "red")))
  "Face for new reports that nobody has answered.")

(defface debbugs-handled '((t (:foreground "ForestGreen")))
  "Face for new reports that nobody has answered.")

(defface debbugs-stale '((t (:foreground "orange")))
  "Face for new reports that nobody has answered.")

(defface debbugs-done '((t (:foreground "DarkGrey")))
  "Face for closed bug reports.")

(defun debbugs-emacs (severities &optional package suppress-done archivedp)
  "List all outstanding Emacs bugs."
  (interactive
   (list
    (completing-read "Severity: "
		     '("important" "normal" "minor" "wishlist")
		     nil t "normal")))
  (unless (consp severities)
    (setq severities (list severities)))
  (pop-to-buffer (get-buffer-create "*Emacs Bugs*"))
  (debbugs-mode)
  (let ((debbugs-port "gnu.org")
	(buffer-read-only nil)
	(ids nil)
	(default 500))
    (dolist (severity severities)
      (setq ids (nconc ids
		       (debbugs-get-bugs :package (or package "emacs")
					 :severity severity
					 :archive (if archivedp
						      "1" "0")))))
    (erase-buffer)

    (when (> (length ids) default)
      (let* ((cursor-in-echo-area nil)
	     (input
	      (read-string
	       (format
		"How many reports (available %d, default %d): "
		(length ids) default)
	       nil
	       nil
	       (number-to-string default))))
	(setq ids (last (sort ids '<) (string-to-number input)))))

    (dolist (status (sort (apply 'debbugs-get-status ids)
			  (lambda (s1 s2)
			    (< (cdr (assq 'id s1))
			       (cdr (assq 'id s2))))))
      (when (or (not suppress-done)
		(not (equal (cdr (assq 'pending status)) "done")))
	(let ((address (mail-header-parse-address
			(decode-coding-string (cdr (assq 'originator status))
					      'utf-8)))
	      (subject (decode-coding-string (cdr (assq 'subject status))
					     'utf-8)))
	  (setq address
		;; Prefer the name over the address.
		(or (cdr address)
		    (car address)))
	  (insert
	   (format "%5d %-20s [%-23s] %s\n"
		   (cdr (assq 'id status))
		   (let ((words
			  (mapconcat
			   'identity
			   (cons (cdr (assq 'severity status))
				 (cdr (assq 'keywords status)))
			   ",")))
		     (unless (equal (cdr (assq 'pending status)) "pending")
		       (setq words
			     (concat words "," (cdr (assq 'pending status)))))
		     (if (> (length words) 20)
			 (propertize (substring words 0 20) 'help-echo words)
		       words))
		   (if (> (length address) 23)
		       (propertize (substring address 0 23) 'help-echo address)
		     address)
		   (propertize subject 'help-echo subject)))
	  (forward-line -1)
	  (put-text-property
	   (+ (point) 5) (+ (point) 26)
	   'face
	   (cond
	    ((equal (cdr (assq 'pending status)) "done")
	     'debbugs-done)
	    ((= (cdr (assq 'date status))
		(cdr (assq 'log_modified status)))
	     'debbugs-new)
	    ((< (- (float-time)
		   (cdr (assq 'log_modified status)))
		(* 60 60 24 4))
	     'debbugs-handled)
	    (t
	     'debbugs-stale)))
	  (forward-line 1)))))
  (goto-char (point-min)))

(defvar debbugs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'debbugs-select-report)
    (define-key map "q" 'kill-buffer)
    (define-key map "s" 'debbugs-toggle-sort)
    map))

(defvar debbugs-sort-state 'number)

(defun debbugs-mode ()
  "Major mode for listing bug reports.

All normal editing commands are switched off.
\\<debbugs-mode-map>

The following commands are available:

\\{debbugs-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'debbugs-mode)
  (setq mode-name "Debbugs")
  (use-local-map debbugs-mode-map)
  (set (make-local-variable 'debbugs-sort-state)
       'number)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defvar debbugs-state-preference
  '((debbugs-new . 1)
    (debbugs-stale . 2)
    (debbugs-handled . 3)
    (debbugs-done . 4)))

(defun debbugs-toggle-sort ()
  "Toggle sorting by age and by state."
  (interactive)
  (beginning-of-line)
  (let ((buffer-read-only nil)
	(current-bug (buffer-substring (point) (+ (point) 5))))
    (goto-char (point-min))
    (setq debbugs-sort-state
	  (if (eq debbugs-sort-state 'number)
	      'state
	    'number))
    (sort-subr
     nil (lambda () (forward-line 1)) 'end-of-line
     (lambda ()
       (if (eq debbugs-sort-state 'number)
	   (string-to-number (buffer-substring (point) (+ (point) 5)))
	 (or (cdr (assq (get-text-property (+ (point) 7) 'face)
			debbugs-state-preference))
	     10))))
    (goto-char (point-min))
    (re-search-forward (concat "^" current-bug) nil t)))

(defun debbugs-select-report ()
  "Select the report on the current line."
  (interactive)
  (let (id)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at " *\\([0-9]+\\)"))
	  (error "No bug report on the current line")
	(setq id (string-to-number (match-string 1)))))
    (gnus-read-ephemeral-emacs-bug-group
     id (cons (current-buffer)
	      (current-window-configuration)))
    (with-current-buffer (window-buffer (selected-window))
      (debbugs-summary-mode 1))))

(defvar debbugs-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" 'debbugs-send-control-message)
    map))

(define-minor-mode debbugs-summary-mode
  "Minor mode for providing a debbugs interface in Gnus summary buffers.

\\{debbugs-summary-mode-map}"
  :lighter " Debbugs" :keymap debbugs-summary-mode-map
  (set (make-local-variable 'gnus-posting-styles)
       '((".*"
	  (eval
	   (with-current-buffer gnus-article-copy
	     (set (make-local-variable 'message-prune-recipient-rules)
		  '((".*@debbugs.*" "emacs-pretest-bug")
		    (".*@debbugs.*" "bug-gnu-emacs")))
	     (set (make-local-variable 'message-alter-recipients-function)
		  (lambda (address)
		    (if (string-match "\\([0-9]+\\)@donarmstrong" (car address))
			(let ((new (format "%s@debbugs.gnu.org"
					   (match-string 1 (car address)))))
			  (cons new new))
		      address)))))))))

(defun debbugs-send-control-message (message)
  "Send a control message for the current bug report.
You can set the severity or add a tag, or close the report.  If
you use the special \"done\" MESSAGE, the report will be marked as
fixed, and then closed."
  (interactive
   (list (completing-read
	  "Control message: "
	  '("important" "normal" "minor" "wishlist"
	    "done"
	    "unarchive" "reopen" "close"
	    "merge" "forcemerge"
	    "patch" "wontfix" "moreinfo" "unreproducible" "fixed" "notabug")
	  nil t)))
  (let* ((subject (mail-header-subject (gnus-summary-article-header)))
	 (id
	  (if (string-match "bug#\\([0-9]+\\)" subject)
	      (string-to-number (match-string 1 subject))
	    (error "No bug number present")))
	 (version
	  (when (member message '("close" "done"))
	    (read-string
	     "Version: "
	     (cond
	      ;; Emacs development versions.
	      ((string-match
		"^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\." emacs-version)
	       (format "%s.%d"
		       (match-string 1 emacs-version)
		       (1+ (string-to-number (match-string 2 emacs-version)))))
	      ;; Emacs release versions.
	      ((string-match
		"^\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)$" emacs-version)
	       (format "%s.%s"
		       (match-string 1 emacs-version)
		       (match-string 2 emacs-version)))
	      (t emacs-version))))))
    (with-temp-buffer
      (insert "To: control@debbugs.gnu.org\n"
	      "From: " (message-make-from) "\n"
	      (format "Subject: control message for bug #%d\n" id)
	      "\n"
	      (cond
	       ((member message '("unarchive" "reopen"))
		(format "%s %d\n" message id))
	       ((member message '("merge" "forcemerge"))
		(format "%s %d %s\n" message id
			(read-string "Merge with bug #: ")))
	       ((equal message "close")
		(format "close %d %s\n" id version))
	       ((equal message "done")
		(format "tags %d fixed\nclose %d %s\n" id id version))
	       ((member message '("important" "normal" "minor" "wishlist"))
		(format "severity %d %s\n" id message))
	       (t
		(format "tags %d %s\n" id message))))
      (funcall send-mail-function))))

(provide 'debbugs-gnu)

;;; TODO:

;; * Widget-oriented bug overview like webDDTs.
;; * Actions on bugs.
;; * Integration into gnus (nnir).

;;; debbugs-gnu.el ends here
