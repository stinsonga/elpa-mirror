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
;;   (autoload 'debbugs-gnu "debbugs-gnu" "" 'interactive)

;; The bug tracker is called interactively by
;;
;;   M-x debbugs-gnu

;; It asks for the severities, for which bugs shall be shown. This can
;; be either just one severity, or a list of severities, separated by
;; comma.  Valid severities are "serious", "important", "normal",
;; "minor" or "wishlist".  Severities "critical" and "grave" are not
;; used, although configured on the GNU bug tracker.  If no severity
;; is given, all bugs are selected.

;; There is also the pseudo severity "tagged", which selects locally
;; tagged bugs.

;; If a prefix is given to the command, more search parameters are
;; asked for, like packages (also a comma separated list, "emacs" is
;; the default), whether archived bugs shall be shown, and whether
;; closed bugs shall be shown.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, you will be asked to
;; change this limit, but please don't increase this number too much.

;; These default values could be changed also by customer options
;; `debbugs-gnu-default-severities', `debbugs-gnu-default-packages'
;; and `debbugs-gnu-default-hits-per-page'.

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

;;   "g": Rescan bugs
;;   "q": Quit the buffer
;;   "s": Toggle bug sorting for age or for state
;;   "x": Toggle suppressing of closed bugs

;; When you visit the related bug messages in Gnus, you could also
;; send control messages by keystroke "C".

;; In the header line of every bug list page, you can toggle sorting
;; per column by selecting a column with the mouse.  The sorting
;; happens as expected for the respective column; sorting in the Title
;; column is depending on whether you are the owner of a bug.

;;; Code:

(require 'debbugs)
(require 'widget)
(require 'tabulated-list)
(eval-when-compile (require 'cl))

(autoload 'widget-convert "wid-edit.el")
(autoload 'gnus-read-ephemeral-emacs-bug-group "gnus-group")
(autoload 'mail-header-subject "nnheader")
(autoload 'gnus-summary-article-header "gnus-sum")
(autoload 'message-make-from "message")

(defgroup debbugs-gnu ()
  "UI for the debbugs.gnu.org bug tracker."
  :group 'debbugs
  :version "24.1")

(defcustom debbugs-gnu-default-severities '("normal")
  "*The list severities bugs are searched for.
\"tagged\" is not a severity but marks locally tagged bugs."
  :group 'debbugs-gnu
  :type '(set (const "serious")
	      (const "important")
	      (const "normal")
	      (const "minor")
	      (const "wishlist")
	      (const "tagged"))
  :version "24.1")

(defcustom debbugs-gnu-default-packages '("emacs")
  "*The list of packages to be searched for."
  :group 'debbugs-gnu
  :type '(set (const "automake")
	      (const "coreutils")
	      (const "emacs")
	      (const "gnus")
	      (const "libtool"))
  :version "24.1")

(defcustom debbugs-gnu-default-hits-per-page 500
  "*The number of bugs shown per page."
  :group 'debbugs-gnu
  :type 'integer
  :version "24.1")

(defface debbugs-gnu-new '((t (:foreground "red")))
  "Face for new reports that nobody has answered.")

(defface debbugs-gnu-handled '((t (:foreground "ForestGreen")))
  "Face for reports that have been modified recently.")

(defface debbugs-gnu-pending '((t (:foreground "MidnightBlue")))
  "Face for reports that have been modified recently.")

(defface debbugs-gnu-stale '((t (:foreground "orange")))
  "Face for reports that have not been touched for a week.")

(defface debbugs-gnu-done '((t (:foreground "DarkGrey")))
  "Face for closed bug reports.")

(defface debbugs-gnu-tagged '((t (:foreground "red")))
  "Face for reports that have been tagged locally.")

(defvar debbugs-gnu-widgets nil)

(defvar debbugs-gnu-widget-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'widget-button-press)
    (define-key map [mouse-1] 'widget-button-press)
    (define-key map [mouse-2] 'widget-button-press)
    map))

(defvar debbugs-gnu-local-tags nil
  "List of bug numbers tagged locally, and kept persistent.")

(defvar debbugs-gnu-persistency-file
  (expand-file-name (locate-user-emacs-file "debbugs"))
  "File name of a persistency store for debbugs variables")

(defun debbugs-gnu-dump-persistency-file ()
  "Function to store debbugs variables persistently."
  (with-temp-file debbugs-gnu-persistency-file
    (insert
     ";; -*- emacs-lisp -*-\n"
     ";; Debbugs tags connection history.  Don't change this file.\n\n"
     (format "(setq debbugs-gnu-local-tags '%S)"
	     (sort (copy-sequence debbugs-gnu-local-tags) '<)))))

(defvar debbugs-gnu-current-severities nil
  "The severities strings to be searched for.")

(defvar debbugs-gnu-current-packages nil
  "The package names to be searched for.")

(defvar debbugs-gnu-current-archive nil
  "Whether to search in the archive.")

(defun debbugs-gnu (severities &optional packages archivedp suppress-done)
  "List all outstanding Emacs bugs."
  (interactive
   (let (archivedp)
     (list
      (completing-read-multiple
       "Severity: "
       (mapcar 'cadr (cdr (get 'debbugs-gnu-default-severities 'custom-type)))
       nil t (mapconcat 'identity debbugs-gnu-default-severities ","))
      ;; The optional parameters are asked only when there is a prefix.
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: "
	   (mapcar 'cadr (cdr (get 'debbugs-gnu-default-packages 'custom-type)))
	   nil t (mapconcat 'identity debbugs-gnu-default-packages ","))
	debbugs-gnu-default-packages)
      (when current-prefix-arg
	(setq archivedp (y-or-n-p "Show archived bugs?")))
      (when (and current-prefix-arg (not archivedp))
	(y-or-n-p "Suppress closed bugs?")))))

  ;; Initialize variables.
  (when (and (file-exists-p debbugs-gnu-persistency-file)
	     (not debbugs-gnu-local-tags))
    (with-temp-buffer
      (insert-file-contents debbugs-gnu-persistency-file)
      (eval (read (current-buffer)))))
  ;; Set lists.
  (unless (consp severities)
    (setq severities (list severities)))
  (unless (consp packages)
    (setq packages (list packages)))

  (setq debbugs-gnu-current-severities severities
	debbugs-gnu-current-packages packages
	debbugs-gnu-current-archive (if archivedp "1" "0")
	debbugs-gnu-widgets nil)

  (let ((hits debbugs-gnu-default-hits-per-page)
	(ids (debbugs-gnu-get-bugs)))

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
	     'debbugs-gnu-widgets
	     (widget-convert
	      'push-button
	      :follow-link 'mouse-face
	      :notify (lambda (widget &rest ignore)
			(debbugs-gnu-show-reports widget))
	      :keymap debbugs-gnu-widget-map
	      :suppress-done suppress-done
	      :buffer-name (format "*Emacs Bugs*<%d>" i)
	      :bug-ids curr-ids
	      :help-echo (format "%d-%d" (car ids) (car (last curr-ids)))
	      :format " %[%v%]"
	      (number-to-string i))
	     'append)
	    (setq ids (last ids (- (length ids) hits))))
	  (debbugs-gnu-show-reports (car debbugs-gnu-widgets)))

      (debbugs-gnu-show-reports
       (widget-convert
	'const
	:suppress-done suppress-done
	:buffer-name "*Emacs Bugs*"
	:bug-ids ids)))))

(defun debbugs-gnu-get-bugs ()
  "Retrieve bugs numbers from debbugs.gnu.org according search criteria."
  (let ((debbugs-port "gnu.org")
	(args `(:archive ,debbugs-gnu-current-archive))
	(ids (when (member "tagged" debbugs-gnu-current-severities)
	       (copy-sequence debbugs-gnu-local-tags))))
    (dolist (severity (delete "tagged" debbugs-gnu-current-severities))
      (when (not (zerop (length severity)))
	(setq args (append args `(:severity ,severity)))))
    (dolist (package debbugs-gnu-current-packages)
      (when (not (zerop (length package)))
	(setq args (append args `(:package ,package)))))
    (sort (nconc ids (apply 'debbugs-get-bugs args)) '<)))

(defvar debbugs-gnu-current-widget nil)

(defvar widget-mouse-face)

(defun debbugs-gnu-show-reports (widget)
  "Show bug reports as given in WIDGET property :bug-ids."
  (pop-to-buffer (get-buffer-create (widget-get widget :buffer-name)))
  (debbugs-gnu-mode)
  (let ((inhibit-read-only t)
	(debbugs-port "gnu.org"))

    (erase-buffer)
    (set (make-local-variable 'debbugs-gnu-current-widget)
	 widget)

    (dolist (status (apply 'debbugs-get-status (widget-get widget :bug-ids)))
      (let* ((id (cdr (assq 'id status)))
	     (words
	      (mapconcat
	       'identity
	       (cons (cdr (assq 'severity status))
		     (cdr (assq 'keywords status)))
	       ","))
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
	(let ((packages (delete "emacs" (cdr (assq 'package status)))))
	  (when packages
	    (setq words (concat words "," (mapconcat 'identity packages ",")))))
	(when (setq merged (cdr (assq 'mergedwith status)))
	  (setq words (format "%s,%s"
			      (if (numberp merged)
				  merged
				(mapconcat 'number-to-string merged ","))
			      words)))
	(add-to-list
	 'tabulated-list-entries
	 (list
	  status
	  (vector
	   (propertize
	    (format "%5d" id)
	    'face
	    ;; Mark tagged bugs.
	    (if (memq id debbugs-gnu-local-tags)
		'debbugs-gnu-tagged
	      'default))
	   (propertize
	    ;; Mark status and age.
	    words
	    'face
	    (cond
	     ((equal (cdr (assq 'pending status)) "done")
	      'debbugs-gnu-done)
	     ((member "pending" (cdr (assq 'keywords status)))
	      'debbugs-gnu-pending)
	     ((= (cdr (assq 'date status))
		 (cdr (assq 'log_modified status)))
	      'debbugs-gnu-new)
	     ((< (- (float-time)
		    (cdr (assq 'log_modified status)))
		 (* 60 60 24 7))
	      'debbugs-gnu-handled)
	     (t
	      'debbugs-gnu-stale)))
	   (propertize
	    ;; Prefer the name over the address.
	    (or (cdr address)
		(car address))
	    'face
	    ;; Mark own submitted bugs.
	    (if (and (stringp (car address))
		     (string-equal (car address) user-mail-address))
		'debbugs-gnu-tagged
	      'default))
	   (propertize
	    subject
	    'face
	    ;; Mark owned bugs.
	    (if (and (stringp owner)
		     (string-equal owner user-mail-address))
		'debbugs-gnu-tagged
	      'default))))
	 'append)))
    (tabulated-list-init-header)
    (tabulated-list-print)

    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun debbugs-gnu-print-entry (list-id cols)
  "Insert a debbugs entry at point.
Used instead of `tabulated-list-print-entry'."
  ;; This shall be in `debbugs-gnu-show-reports'.  But
  ;; `tabulated-list-print' erases the buffer, therefore we do it
  ;; here.  (bug#9047)
  (when (and debbugs-gnu-widgets (= (point) (point-min)))
    (widget-insert "Page:")
    (mapc
     (lambda (obj)
       (if (eq obj debbugs-gnu-current-widget)
	   (widget-put obj :button-face 'widget-button-pressed)
	 (widget-put obj :button-face 'widget-button-face))
       (widget-apply obj :create))
     debbugs-gnu-widgets)
    (widget-insert "\n\n")
    (save-excursion
      (widget-insert "\nPage:")
      (mapc (lambda (obj) (widget-apply obj :create)) debbugs-gnu-widgets)
      (widget-setup)))

  (when (or (not (widget-get debbugs-gnu-current-widget :suppress-done))
	    (not (equal (cdr (assq 'pending list-id)) "done")))
    (let ((beg (point))
	  (pos 0)
	  (id               (aref cols 0))
	  (id-length        (nth 1 (aref tabulated-list-format 0)))
	  (state            (aref cols 1))
	  (state-length     (nth 1 (aref tabulated-list-format 1)))
	  (submitter        (aref cols 2))
	  (submitter-length (nth 1 (aref tabulated-list-format 2)))
	  (title            (aref cols 3))
	  (title-length     (nth 1 (aref tabulated-list-format 3))))
      ;; Insert id.
      (indent-to (- id-length (length id)))
      (insert id)
      ;; Insert state.
      (indent-to (setq pos (+ pos id-length 1)) 1)
      (insert (if (> (length state) state-length)
		  (propertize (substring state 0 state-length)
			      'help-echo state)
		state))
      ;; Insert submitter.
      (indent-to (setq pos (+ pos state-length 1)) 1)
      (insert "[" (if (> (length submitter) (- submitter-length 2))
		      (propertize (substring submitter 0 (- submitter-length 2))
				  'help-echo submitter)
		    submitter))
      (indent-to (+ pos (1- submitter-length)))
      (insert "]")
      ;; Insert title.
      (indent-to (setq pos (+ pos submitter-length 1)) 1)
      (insert (propertize title 'help-echo title))
      ;; Add properties.
      (add-text-properties
       beg (point) `(tabulated-list-id ,list-id mouse-face ,widget-mouse-face))
      (insert ?\n))))

(defvar debbugs-gnu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "\r" 'debbugs-gnu-select-report)
    (define-key map [mouse-1] 'debbugs-gnu-select-report)
    (define-key map [mouse-2] 'debbugs-gnu-select-report)
    (define-key map "q" 'bury-buffer)
    (define-key map "s" 'debbugs-gnu-toggle-sort)
    (define-key map "t" 'debbugs-gnu-toggle-tag)
    (define-key map "d" 'debbugs-gnu-display-status)
    (define-key map "g" 'debbugs-gnu-rescan)
    (define-key map "x" 'debbugs-gnu-toggle-suppress-done)
    (define-key map "C" 'debbugs-gnu-send-control-message)
    map))

(defun debbugs-gnu-rescan ()
  "Rescan the current set of bug reports."
  (interactive)

  ;; The last page will be provided with new bug ids.
  ;; TODO: Do it also for the other pages.
  (when (and debbugs-gnu-widgets
	     (eq debbugs-gnu-current-widget (car (last debbugs-gnu-widgets))))
    (let ((first-id (car (widget-get debbugs-gnu-current-widget :bug-ids)))
	  (last-id  (car
		     (last (widget-get debbugs-gnu-current-widget :bug-ids))))
	  (ids (debbugs-gnu-get-bugs)))

      (while (and (<= first-id last-id) (not (memq first-id ids)))
	(setq first-id (1+ first-id)))

      (when (<= first-id last-id)
	(widget-put debbugs-gnu-current-widget :bug-ids (memq first-id ids)))))

  ;; Refresh the buffer.  `save-excursion' does not work, so we
  ;; remember the position.
  (let ((pos (point)))
    (debbugs-gnu-show-reports debbugs-gnu-current-widget)
    (goto-char pos)))

(defvar debbugs-gnu-sort-state 'number)

(define-derived-mode debbugs-gnu-mode tabulated-list-mode "Debbugs"
  "Major mode for listing bug reports.

All normal editing commands are switched off.
\\<debbugs-gnu-mode-map>

The following commands are available:

\\{debbugs-gnu-mode-map}"
  (set (make-local-variable 'debbugs-gnu-sort-state)
       'number)
  (setq tabulated-list-format [("Id"         5 debbugs-gnu-sort-id)
			       ("State"     20 debbugs-gnu-sort-state)
			       ("Submitter" 25 t)
			       ("Title"     10 debbugs-gnu-sort-title)])
  (setq tabulated-list-sort-key (cons "Id" nil))
  (setq tabulated-list-printer 'debbugs-gnu-print-entry)
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun debbugs-gnu-sort-id (s1 s2)
  (< (cdr (assq 'id (car s1)))
     (cdr (assq 'id (car s2)))))

(defconst debbugs-gnu-state-preference
  '((debbugs-gnu-new . 1)
    (debbugs-gnu-stale . 2)
    (debbugs-gnu-handled . 3)
    (debbugs-gnu-done . 4)
    (debbugs-gnu-pending . 5)))

(defun debbugs-gnu-get-state-preference (face-string)
  (or (cdr (assq (get-text-property 0 'face face-string)
		 debbugs-gnu-state-preference))
      10))

(defconst debbugs-gnu-severity-preference
  '(("serious" . 1)
    ("important" . 2)
    ("normal" . 3)
    ("minor" . 4)
    ("wishlist" . 5)))

(defun debbugs-gnu-get-severity-preference (state)
  (or (cdr (assoc (cdr (assq 'severity state))
		  debbugs-gnu-severity-preference))
      10))

(defun debbugs-gnu-sort-state (s1 s2)
  (let ((id1 (cdr (assq 'id (car s1))))
	(age1 (debbugs-gnu-get-state-preference (aref (nth 1 s1) 1)))
	(id2 (cdr (assq 'id (car s2))))
	(age2 (debbugs-gnu-get-state-preference (aref (nth 1 s2) 1))))
    (cond
     ;; Tagged bugs go to the end.
     ((and (not (memq id1 debbugs-gnu-local-tags))
	   (memq id2 debbugs-gnu-local-tags))
      t)
     ((and (memq id1 debbugs-gnu-local-tags)
	   (not (memq id2 debbugs-gnu-local-tags)))
      nil)
     ;; Then, we check the age of the bugs.
     ((< age1 age2)
      t)
     ((> age1 age2)
      nil)
     ;; If they have the same age, we check for severity.
     ((< (debbugs-gnu-get-severity-preference (car s1))
	 (debbugs-gnu-get-severity-preference (car s2)))
      t)
     (t nil))))

(defun debbugs-gnu-sort-title (s1 s2)
  (let ((owner (if (cdr (assq 'owner (car s1)))
		   (car (mail-header-parse-address
			 (decode-coding-string (cdr (assq 'owner (car s1)))
					       'utf-8))))))
    (and (stringp owner)
	 (string-equal owner user-mail-address))))

(defun debbugs-gnu-toggle-sort ()
  "Toggle sorting by age and by state."
  (interactive)
  (if (eq debbugs-gnu-sort-state 'number)
      (progn
	(setq debbugs-gnu-sort-state 'state)
	(setq tabulated-list-sort-key (cons "Id" nil)))
    (setq debbugs-gnu-sort-state 'number)
    (setq tabulated-list-sort-key (cons "State" nil)))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun debbugs-gnu-toggle-tag ()
  "Toggle tag of the report in the current line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((inhibit-read-only t)
	  (id (debbugs-gnu-current-id)))
      (if (memq id debbugs-gnu-local-tags)
	  (progn
	    (setq debbugs-gnu-local-tags (delq id debbugs-gnu-local-tags))
	    (put-text-property (point) (+ (point) 5) 'face 'default))
	(add-to-list 'debbugs-gnu-local-tags id)
	(put-text-property
	 (+ (point) (- 5 (length (number-to-string id)))) (+ (point) 5)
	 'face 'debbugs-gnu-tagged))))
  (debbugs-gnu-dump-persistency-file))

(defun debbugs-gnu-toggle-suppress-done ()
  "Suppress bugs marked as done."
  (interactive)
  (widget-put debbugs-gnu-current-widget :suppress-done
	      (not (widget-get debbugs-gnu-current-widget :suppress-done)))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defvar debbugs-gnu-bug-number nil)
(defvar debbugs-gnu-subject nil)

(defun debbugs-gnu-current-id (&optional noerror)
  (or (cdr (assq 'id (debbugs-gnu-current-status)))
      (and (not noerror)
	   (error "No bug on the current line"))))

(defun debbugs-gnu-current-status ()
  (get-text-property (line-beginning-position) 'tabulated-list-id))

(defun debbugs-gnu-display-status (status)
  "Display the status of the report on the current line."
  (interactive (list (debbugs-gnu-current-status)))
  (pop-to-buffer "*Bug Status*")
  (erase-buffer)
  (pp status (current-buffer))
  (goto-char (point-min)))

(defun debbugs-gnu-select-report ()
  "Select the report on the current line."
  (interactive)
  ;; We open the report messages.
  (let* ((status (debbugs-gnu-current-status))
	 (id (cdr (assq 'id status)))
	 (merged (cdr (assq 'mergedwith status))))
    (gnus-read-ephemeral-emacs-bug-group
     (cons id (if (listp merged)
		  merged
		(list merged)))
     (cons (current-buffer)
	   (current-window-configuration)))
    (with-current-buffer (window-buffer (selected-window))
      (set (make-local-variable 'debbugs-gnu-bug-number) id)
      (set (make-local-variable 'debbugs-gnu-subject)
	   (format "Re: bug#%d: %s" id (cdr (assq 'subject status))))
      (debbugs-gnu-summary-mode 1))))

(defvar debbugs-gnu-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" 'debbugs-gnu-send-control-message)
    map))

(defvar gnus-posting-styles)

(define-minor-mode debbugs-gnu-summary-mode
  "Minor mode for providing a debbugs interface in Gnus summary buffers.

\\{debbugs-gnu-summary-mode-map}"
  :lighter " Debbugs" :keymap debbugs-gnu-summary-mode-map
  (set (make-local-variable 'gnus-posting-styles)
       `((".*"
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
		      address)))
	     ;; `gnus-posting-styles' is eval'ed after
	     ;; `message-simplify-subject'.  So we cannot use m-s-s.
	     (setq subject ,debbugs-gnu-subject)))))))

(defun debbugs-gnu-send-control-message (message &optional reverse)
  "Send a control message for the current bug report.
You can set the severity or add a tag, or close the report.  If
you use the special \"done\" MESSAGE, the report will be marked as
fixed, and then closed.

If given a prefix, and given a tag to set, the tag will be
removed instead."
  (interactive
   (list (completing-read
	  "Control message: "
	  '("serious" "important" "normal" "minor" "wishlist"
	    "done" "donenotabug" "donewontfix" "doneunreproducible"
	    "unarchive" "reopen" "close"
	    "merge" "forcemerge"
	    "owner" "noowner"
	    "invalid"
	    "patch" "wontfix" "moreinfo" "unreproducible" "fixed" "notabug"
	    "pending" "help" "security" "confirmed")
	  nil t)
	 current-prefix-arg))
  (let* ((id (or debbugs-gnu-bug-number	; Set on group entry.
		 (debbugs-gnu-current-id)))
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
	       ((member message '("donenotabug" "donewontfix"
				  "doneunreproducible"))
		(format "tags %d %s\nclose %d\n" id (substring message 4) id))
	       ((member message '("serious" "important" "normal"
				  "minor" "wishlist"))
		(format "severity %d %s\n" id message))
	       ((equal message "invalid")
		(format "tags %d notabug\ntags %d wontfix\nclose %d\n"
			id id id))
	       (t
		(format "tags %d%s %s\n"
			id (if reverse " -" "")
			message))))
      (funcall send-mail-function))))

(provide 'debbugs-gnu)

;;; TODO:

;;; debbugs-gnu.el ends here
