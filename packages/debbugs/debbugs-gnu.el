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
(require 'widget)
(eval-when-compile (require 'cl))

(autoload 'gnus-read-ephemeral-emacs-bug-group "gnus-group")
(autoload 'mail-header-subject "nnheader")
(autoload 'gnus-summary-article-header "gnus-sum")
(autoload 'message-make-from "message")

(defface debbugs-new '((t (:foreground "red")))
  "Face for new reports that nobody has answered.")

(defface debbugs-handled '((t (:foreground "ForestGreen")))
  "Face for new reports that have been modified recently.")

(defface debbugs-stale '((t (:foreground "orange")))
  "Face for new reports that nobody has answered.")

(defface debbugs-done '((t (:foreground "DarkGrey")))
  "Face for closed bug reports.")

(defface debbugs-tagged '((t (:foreground "red")))
  "Face for reports that have been tagged locally.")

(defvar debbugs-widgets nil)

(defvar debbugs-widget-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'widget-button-press)
    (define-key map [mouse-1] 'widget-button-press)
    (define-key map [mouse-2] 'widget-button-press)
    map))

(defvar debbugs-local-tags nil
  "List of bug numbers tagged locally, and kept persistent.")

(defvar debbugs-persistency-file
  (expand-file-name (locate-user-emacs-file "debbugs"))
  "File name of a persistency store for debbugs variables")

(defun debbugs-dump-persistency-file ()
  "Function to store debbugs variables persistently."
  (ignore-errors
    (with-temp-buffer
      (insert
       ";; -*- emacs-lisp -*-\n"
       ";; Debbugs tags connection history.  Don't change this file.\n\n"
       (format "(setq debbugs-local-tags '%S)" (sort debbugs-local-tags '<)))
      (write-region
       (point-min) (point-max) debbugs-persistency-file))))

;; Save variables.
(unless noninteractive
  (add-hook 'kill-emacs-hook 'debbugs-dump-persistency-file))

(defvar debbugs-package nil
  "The package name to be searched for.")

(defvar debbugs-severities nil
  "The severities strings to be searched for.")

(defvar debbugs-archive nil
  "The archive flag to be searched for.")

(defun debbugs-emacs (severities &optional package suppress-done archivedp)
  "List all outstanding Emacs bugs."
  (interactive
   (list
    (completing-read "Severity: "
		     '("important" "normal" "minor" "wishlist")
		     nil t "normal")))
  ;; Initialize variables.
  (when (and (file-exists-p debbugs-persistency-file)
	     (not debbugs-local-tags))
    (with-temp-buffer
      (insert-file-contents debbugs-persistency-file)
      (eval (read (current-buffer)))))
  (unless (consp severities)
    (setq severities (list severities)))

  (setq debbugs-package (or package "emacs")
	debbugs-severities severities
	debbugs-archive (if archivedp "1" "0")
	debbugs-widgets nil)

  (let ((debbugs-port "gnu.org")
	(default 500)
	ids)
    (dolist (severity debbugs-severities)
      (setq ids (nconc ids
		       (debbugs-get-bugs :package debbugs-package
					 :severity severity
					 :archive debbugs-archive))))
    (setq ids (sort ids '<))

    (if (> (length ids) default)
	(let ((cursor-in-echo-area nil))
	  (setq default
		(string-to-number
		 (read-string
		  (format
		   "How many reports (available %d, default %d): "
		   (length ids) default)
		  nil
		  nil
		  (number-to-string default))))))

    (if (> (length ids) default)
	(let ((i 0)
	      curr-ids)
	  (while ids
	    (setq i (1+ i)
		  curr-ids (butlast ids (- (length ids) default)))
	    (add-to-list
	     'debbugs-widgets
	     (widget-convert
	      'push-button
	      :follow-link 'mouse-face
	      :notify (lambda (widget &rest ignore)
			(debbugs-show-reports widget))
	      :keymap debbugs-widget-map
	      :suppress-done suppress-done
	      :buffer-name (format "*Emacs Bugs*<%d>" i)
	      :bug-ids curr-ids
	      :help-echo (format "%d-%d" (car ids) (car (last curr-ids)))
	      :format " %[%v%]"
	      (number-to-string i))
	     'append)
	    (setq ids (last ids (- (length ids) default))))
	  (debbugs-show-reports (car debbugs-widgets)))

      (debbugs-show-reports
       (widget-convert
	'const
	:suppress-done suppress-done
	:buffer-name "*Emacs Bugs*"
	:bug-ids ids)))))

(defvar debbugs-current-widget nil)

(defun debbugs-show-reports (widget)
  "Show bug reports as given in WIDGET property :bug-ids."
  (pop-to-buffer (get-buffer-create (widget-get widget :buffer-name)))
  (debbugs-mode)
  (let ((inhibit-read-only t)
	(suppress-done (widget-get widget :suppress-done)))
    (erase-buffer)

    (when debbugs-widgets
      (widget-insert "Page:")
      (mapc
       (lambda (obj)
	 (if (eq obj widget)
	     (widget-put obj :button-face 'widget-button-pressed)
	   (widget-put obj :button-face 'widget-button-face))
	 (widget-apply obj :create))
       debbugs-widgets)
      (widget-insert "\n\n"))

    (dolist (status (sort (apply 'debbugs-get-status
				 (widget-get widget :bug-ids))
			  (lambda (s1 s2)
			    (< (cdr (assq 'id s1))
			       (cdr (assq 'id s2))))))
      (when (or (not suppress-done)
		(not (equal (cdr (assq 'pending status)) "done")))
	(let* ((id (cdr (assq 'id status)))
	       (words
		(mapconcat
		 'identity
		 (cons (cdr (assq 'severity status))
		       (cdr (assq 'keywords status)))
		 ","))
	       (face (cond
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
	       (address (mail-header-parse-address
			 (decode-coding-string (cdr (assq 'originator status))
					       'utf-8)))
	       (owner (if (cdr (assq 'owner status))
			  (car (mail-header-parse-address
				(decode-coding-string (cdr (assq 'owner status))
						      'utf-8)))))
	       (subject (decode-coding-string (cdr (assq 'subject status))
					      'utf-8))
	       merged)
	  (unless (equal (cdr (assq 'pending status)) "pending")
	    (setq words
		  (concat words "," (cdr (assq 'pending status)))))
	  (when (setq merged (cdr (assq 'mergedwith status)))
	    (setq words (format "%s,%s"
				(if (numberp merged)
				    merged
				  (mapconcat 'number-to-string merged ","))
				words)))
	  (setq words (propertize words 'face face))
	  (setq address
		(propertize
		 ;; Prefer the name over the address.
		 (or (cdr address)
		     (car address))
		 'face
		 ;; Mark own submitted bugs.
		 (if (and (stringp (car address))
			  (string-equal (car address) user-mail-address))
		     'debbugs-tagged
		   'default)))
	  (insert
	   (format "%5d %-20s [%-23s] %s\n"
		   id
		   (if (> (length words) 20)
		       (propertize (substring words 0 20) 'help-echo words)
		     words)
		   (if (> (length address) 23)
		       (propertize (substring address 0 23) 'help-echo address)
		     address)
		   ;; Mark owned bugs.
		   (if (and (stringp owner)
			    (string-equal owner user-mail-address))
		       (propertize subject
				   'face 'debbugs-tagged 'help-echo subject)
		     (propertize subject 'help-echo subject))))
	  (forward-line -1)
	  (put-text-property (point) (1+ (point)) 'debbugs-status status)
	  (put-text-property
	   (point-at-bol) (point-at-eol) 'mouse-face widget-mouse-face)
	  (when (memq id debbugs-local-tags)
	    (put-text-property
	     (+ (point) (- 5 (length (number-to-string id)))) (+ (point) 5)
	     'face 'debbugs-tagged))
	  (forward-line 1))))

    (when debbugs-widgets
      (widget-insert "\nPage:")
      (mapc (lambda (obj) (widget-apply obj :create)) debbugs-widgets)
      (widget-setup))

    (set-buffer-modified-p nil)
    (set (make-local-variable 'debbugs-current-widget)
	 widget)
    (goto-char (point-min))))

(defvar debbugs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'debbugs-select-report)
    (define-key map [mouse-1] 'debbugs-select-report)
    (define-key map [mouse-2] 'debbugs-select-report)
    (define-key map "q" 'kill-buffer)
    (define-key map "s" 'debbugs-toggle-sort)
    (define-key map "t" 'debbugs-toggle-tag)
    (define-key map "d" 'debbugs-display-status)
    (define-key map "g" 'debbugs-rescan)
    (define-key map "x" 'debbugs-suppress-done)
    (define-key map "C" 'debbugs-send-control-message)
    map))

(defun debbugs-rescan ()
  "Rescan the current set of bug reports."
  (interactive)

  ;; The last page will be provided with new bug ids.
  ;; TODO: Do it also for the other pages.
  (when (and debbugs-widgets
	     (eq debbugs-current-widget (car (last debbugs-widgets))))
    (let ((debbugs-port "gnu.org")
	  (first-id (car (widget-get debbugs-current-widget :bug-ids)))
	  (last-id  (car (last (widget-get debbugs-current-widget :bug-ids))))
	  ids)
      (dolist (severity debbugs-severities)
	(setq ids (nconc ids
			 (debbugs-get-bugs :package debbugs-package
					   :severity severity
					   :archive debbugs-archive))))
      (setq ids (sort ids '<))

      (while (and (<= first-id last-id) (not (memq first-id ids)))
	(setq first-id (1+ first-id)))

      (when (<= first-id last-id)
	(widget-put debbugs-current-widget :bug-ids (memq first-id ids)))))

  ;; Refresh the buffer.  `save-excursion' does not work, so we
  ;; remember the position.
  (let ((pos (point)))
    (debbugs-show-reports debbugs-current-widget)
    (goto-char pos)))

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
	(before-change-functions nil)
	(current-bug (debbugs-current-id t))
	(start-point (point)))
    (setq debbugs-sort-state
	  (if (eq debbugs-sort-state 'number)
	      'state
	    'number))
    (goto-char (point-min))
    (while (and (not (eobp))
		(not (get-text-property (point) 'debbugs-status)))
      (forward-line 1))
    (save-restriction
      (narrow-to-region
       (point)
       (progn
	 (goto-char (point-max))
	 (beginning-of-line)
	 (while (and (not (bobp))
		     (not (get-text-property (point) 'debbugs-status)))
	   (forward-line -1))
	 (forward-line 1)
	 (point)))
      (goto-char (point-min))
      (sort-subr
       nil (lambda () (forward-line 1)) 'end-of-line
       (lambda ()
	 (let ((id (debbugs-current-id)))
	   (if (eq debbugs-sort-state 'number)
	       id
	     ;; Sort the tagged ones at the end.
	     (or (and (memq id debbugs-local-tags)
		      20)
		 (cdr (assq (get-text-property (+ (point) 7) 'face)
			    debbugs-state-preference))
		 10))))))
    (if (not current-bug)
	(goto-char start-point)
      (goto-char (point-min))
      (re-search-forward (format "^%d" current-bug) nil t))))

(defun debbugs-toggle-tag ()
  "Toggle tag of the report in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t)
	  (id (debbugs-current-id)))
      (if (memq id debbugs-local-tags)
	  (progn
	    (setq debbugs-local-tags (delq id debbugs-local-tags))
	    (put-text-property (point) (+ (point) 5) 'face 'default))
	(add-to-list 'debbugs-local-tags id)
	(put-text-property
	 (+ (point) (- 5 (length (number-to-string id)))) (+ (point) 5)
	 'face 'debbugs-tagged)))))

(defun debbugs-suppress-done ()
  "Suppress bugs marked as done."
  (interactive)
  (save-excursion
    (unless (widget-get debbugs-current-widget :suppress-done)
      (let ((inhibit-read-only t))
	(widget-put debbugs-current-widget :suppress-done t)
	(beginning-of-buffer)
	(while (and (not (eobp))
		    (not (get-text-property (point) 'debbugs-status)))
	  (forward-line 1))
	(while  (and (not (eobp))
		     (get-text-property (point) 'debbugs-status))
	  (if (equal (cdr (assq 'pending (debbugs-current-status))) "done")
	      (kill-region (point) (progn (forward-line 1) (point)))
	    (forward-line 1)))))))

(defvar debbugs-bug-number nil)

(defun debbugs-current-id (&optional noerror)
  (or (cdr (assq 'id (debbugs-current-status)))
      (and (not noerror)
	   (error "No bug on the current line"))))

(defun debbugs-current-status ()
  (get-text-property (line-beginning-position)
		     'debbugs-status))

(defun debbugs-display-status (status)
  "Display the status of the report on the current line."
  (interactive (list (debbugs-current-status)))
  (pop-to-buffer "*Bug Status*")
  (erase-buffer)
  (pp status (current-buffer))
  (goto-char (point-min)))

(defun debbugs-select-report ()
  "Select the report on the current line."
  (interactive)
  ;; We open the report messages.
  (let* ((status (debbugs-current-status))
	 (id (cdr (assq 'id status)))
	 (merged (cdr (assq 'mergedwith status))))
    (gnus-read-ephemeral-emacs-bug-group
     (cons id (if (listp merged)
		  merged
		(list merged)))
     (cons (current-buffer)
	   (current-window-configuration)))
    (with-current-buffer (window-buffer (selected-window))
      (debbugs-summary-mode 1)
      (set (make-local-variable 'debbugs-bug-number) id))))

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
		    (".*@debbugs.*" "bug-gnu-emacs")
		    ("[0-9]+@debbugs.*" "submit@debbugs.gnu.org")))
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
	    "owner" "noowner"
	    "patch" "wontfix" "moreinfo" "unreproducible" "fixed" "notabug")
	  nil t)))
  (let* ((id (or debbugs-bug-number	; Set on group entry.
		 (debbugs-current-id)))
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
	       ((member message '("unarchive" "reopen" "noowner"))
		(format "%s %d\n" message id))
	       ((member message '("merge" "forcemerge"))
		(format "%s %d %s\n" message id
			(read-string "Merge with bug #: ")))
	       ((equal message "owner")
		(format "owner %d !\n" id))
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
