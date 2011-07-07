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

;; This package provides an interface to bug reports which are located
;; on the GNU bug tracker debbugs.gnu.org.  It's main purpose is to
;; show and manipulate bug reports from Emacs, but it could be used
;; also for other GNU projects which use the same bug tracker.

;; If you have `debbugs-gnu.el' in your load-path, you could enable
;; the bug tracker command by the following line in your ~/.emacs
;;
;;   (autoload 'debbugs-emacs "debbugs-gnu" "" 'interactive)

;; The bug tracker is called interactively by
;;
;;   M-x debbugs-emacs

;; It asks for the severities, for which bugs shall be shown. This can
;; be either just one severity, or a list of severities, separated by
;; comma.  Valid severities are "important", "normal", "minor" or
;; "wishlist".

;; If a prefix is given, more search parameters are asked for, like
;; packages (also a comma separated list, "emacs" is the default),
;; whether archived bugs shall be shown, and whether closed bugs shall
;; be shown.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, you will be asked to
;; change this limit, but please don't increase this number too much.

;; These default values could be changed also by customer options
;; `debbugs-default-severities', `debbugs-default-packages' and
;; `debbugs-default-hits-per-page'.

;; The command creates one or more pages of bug lists.  Every bug is
;; shown in one line, including the bug number, the status (combining
;; merged bug numbers, keywords and severities), the name of the
;; submitter, and the title of the bug.  On every bug line you could
;; apply the following actions by the following keystrokes:

;;   RET: Show corresponding messages in Gnus
;;   "C": Send a control message
;;   "t": Mark the bug locally as tagged
;;   "d": Show bug attributes

;; Furthermore, you could apply the global actions

;;   "s": Toggle bug sorting
;;   "g": Rescan bugs
;;   "x": Suppress closed bugs
;;   "q": Quit the buffer

;; When you visit the related bug messages in Gnus, you could also
;; send control messages by keystroke "C".

;;; Code:

(require 'debbugs)
(require 'widget)
(eval-when-compile (require 'cl))

(autoload 'gnus-read-ephemeral-emacs-bug-group "gnus-group")
(autoload 'mail-header-subject "nnheader")
(autoload 'gnus-summary-article-header "gnus-sum")
(autoload 'message-make-from "message")

(defgroup debbugs-gnu ()
  "UI for the debbugs.gnu.org bug tracker."
  :group 'debbugs)

(defcustom debbugs-default-severities '("normal")
  "*The list severities bugs are searched for."
  :group 'debbugs-gnu
  :type '(set (const "important")
	      (const "normal")
	      (const "minor")
	      (const "wishlist"))
  :version "24.1")

(defcustom debbugs-default-packages '("emacs")
  "*The list of packages to be searched for."
  :group 'debbugs-gnu
  :type '(set (const "automake")
	      (const "coreutils")
	      (const "emacs")
	      (const "gnus")
	      (const "libtool"))
  :version "24.1")

(defcustom debbugs-default-hits-per-page 500
  "*The number of bugs shown per page."
  :group 'debbugs-gnu
  :type 'integer
  :version "24.1")

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
       (format "(setq debbugs-local-tags '%S)"
	       (sort (copy-sequence debbugs-local-tags) '<)))
      (write-region
       (point-min) (point-max) debbugs-persistency-file))))

;; Save variables.
(unless noninteractive
  (add-hook 'kill-emacs-hook 'debbugs-dump-persistency-file))

(defvar debbugs-current-severities nil
  "The severities strings to be searched for.")

(defvar debbugs-current-packages nil
  "The package names to be searched for.")

(defvar debbugs-current-archive nil
  "Whether to search in the archive.")

(defun debbugs-emacs (severities &optional packages archivedp suppress-done)
  "List all outstanding Emacs bugs."
  (interactive
   (let (archivedp)
     (list
      (completing-read-multiple
       "Severity: "
       (mapcar 'cadr (cdr (get 'debbugs-default-severities 'custom-type)))
       nil t (mapconcat 'identity debbugs-default-severities ","))
      ;; The optional parameters are asked only when there is a prefix.
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: "
	   (mapcar 'cadr (cdr (get 'debbugs-default-packages 'custom-type)))
	   nil t (mapconcat 'identity debbugs-default-packages ","))
	debbugs-default-packages)
      (when current-prefix-arg
	(setq archivedp (y-or-n-p "Show archived bugs?")))
      (when (and current-prefix-arg (not archivedp))
	(y-or-n-p "Suppress closed bugs?")))))

  ;; Initialize variables.
  (when (and (file-exists-p debbugs-persistency-file)
	     (not debbugs-local-tags))
    (with-temp-buffer
      (insert-file-contents debbugs-persistency-file)
      (eval (read (current-buffer)))))
  ;; Set lists.
  (unless (consp severities)
    (setq severities (list severities)))
  (unless (consp packages)
    (setq packages (list packages)))

  (setq debbugs-current-severities severities
	debbugs-current-packages packages
	debbugs-current-archive (if archivedp "1" "0")
	debbugs-widgets nil)

  (let ((debbugs-port "gnu.org")
	(hits debbugs-default-hits-per-page)
	ids)
    (dolist (severity debbugs-current-severities)
      (dolist (package debbugs-current-packages)
	(setq ids (nconc ids
			 (debbugs-get-bugs :package package
					   :severity severity
					   :archive debbugs-current-archive)))))
    (setq ids (sort ids '<))

    (if (> (length ids) hits)
	(let ((cursor-in-echo-area nil))
	  (setq hits
		(string-to-number
		 (read-string
		  (format
		   "How many reports (available %d, default %d): "
		   (length ids) hits)
		  nil
		  nil
		  (number-to-string hits))))))

    (if (> (length ids) hits)
	(let ((i 0)
	      curr-ids)
	  (while ids
	    (setq i (1+ i)
		  curr-ids (butlast ids (- (length ids) hits)))
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
	    (setq ids (last ids (- (length ids) hits))))
	  (debbugs-show-reports (car debbugs-widgets)))

      (debbugs-show-reports
       (widget-convert
	'const
	:suppress-done suppress-done
	:buffer-name "*Emacs Bugs*"
	:bug-ids ids)))))

(defvar debbugs-current-widget nil)

(defvar widget-mouse-face)

(defun debbugs-show-reports (widget)
  "Show bug reports as given in WIDGET property :bug-ids."
  (pop-to-buffer (get-buffer-create (widget-get widget :buffer-name)))
  (debbugs-mode)
  (let ((inhibit-read-only t)
	(debbugs-port "gnu.org")
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
    (define-key map "q" 'bury-buffer)
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
      (dolist (severity debbugs-current-severities)
	(dolist (package debbugs-current-packages)
	  (setq ids
		(nconc ids
		       (debbugs-get-bugs :package package
					 :severity severity
					 :archive debbugs-current-archive)))))
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
	(goto-char (point-min))
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

(defvar gnus-posting-styles)

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
		    ("[0-9]+@debbugs.*" "submit@debbugs.gnu.org")
		    ("[0-9]+@debbugs.*" "quiet@debbugs.gnu.org")))
	     (set (make-local-variable 'message-alter-recipients-function)
		  (lambda (address)
		    (if (string-match "\\([0-9]+\\)@donarmstrong" (car address))
			(let ((new (format "%s@debbugs.gnu.org"
					   (match-string 1 (car address)))))
			  (cons new new))
		      address)))))))))

(defun debbugs-send-control-message (message &optional reverse)
  "Send a control message for the current bug report.
You can set the severity or add a tag, or close the report.  If
you use the special \"done\" MESSAGE, the report will be marked as
fixed, and then closed.

If given a prefix, and given a tag to set, the tag will be
removed instead."
  (interactive
   (list (completing-read
	  "Control message: "
	  '("important" "normal" "minor" "wishlist"
	    "done"
	    "unarchive" "reopen" "close"
	    "merge" "forcemerge"
	    "owner" "noowner"
	    "patch" "wontfix" "moreinfo" "unreproducible" "fixed" "notabug")
	  nil t)
	 current-prefix-arg))
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
		(format "tags %d%s %s\n"
			id (if reverse " -" "")
			message))))
      (funcall send-mail-function))))

(provide 'debbugs-gnu)

;;; TODO:

;; * Widget-oriented bug overview like webDDTs.
;; * Actions on bugs.
;; * Integration into gnus (nnir).

;;; debbugs-gnu.el ends here
