;; gobject-align.el --- GObject-style alignment -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Daiki Ueno <ueno@gnu.org>

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GObject, C, coding style

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cc-mode)
(require 'cl-lib)

(defvar gobject-align-max-column 80)

(defvar gobject-align-identifier-start-column nil)
(make-variable-buffer-local 'gobject-align-identifier-start-column)

(defvar gobject-align-arglist-start-column nil)
(make-variable-buffer-local 'gobject-align-arglist-start-column)

(defvar gobject-align-arglist-identifier-start-column nil)
(make-variable-buffer-local 'gobject-align-arglist-identifier-start-column)

(cl-defstruct (gobject-align--argument
	       (:constructor nil)
	       (:constructor gobject-align--make-argument (type-start
							   type-end
							   identifier-start
							   identifier-end))
	       (:copier nil)
	       (:predicate nil))
  (type-start nil :read-only t)
  (type-end nil :read-only t)
  (identifier-start nil :read-only t)
  (identifier-end nil :read-only t))

(defun gobject-align--marker-column (marker)
  (save-excursion
    (goto-char marker)
    (current-column)))

(defun gobject-align--indent-to-column (column)
  ;; Prefer 'char **foo' than 'char ** foo'
  (when (looking-back "\*+" nil t)
    (setq column (- column (- (match-end 0) (match-beginning 0))))
    (goto-char (match-beginning 0)))
  (let (indent-tabs-mode)
    (indent-to-column column)))

(defun gobject-align--argument-type-width (arg)
  (- (gobject-align--marker-column (gobject-align--argument-type-end arg))
     (gobject-align--marker-column (gobject-align--argument-type-start arg))))

(defun gobject-align--arglist-identifier-start-column (arglist start-column)
  (let ((column start-column)
	argument-column)
    (dolist (argument arglist)
      (setq argument-column (+ start-column
			       (gobject-align--argument-type-width argument)))
      (when (gobject-align--argument-identifier-start argument)
	(save-excursion
	  (goto-char (gobject-align--argument-identifier-start argument))
	  (when (eq (preceding-char) ? )
	    (setq argument-column (1+ argument-column)))))
      (when (> argument-column column)
	(setq column argument-column)))
    column))

(defun gobject-align--argument-identifier-width (argument)
  (if (gobject-align--argument-identifier-start argument)
      (- (gobject-align--marker-column
	  (gobject-align--argument-identifier-end argument))
	 (gobject-align--marker-column
	  (gobject-align--argument-identifier-start argument)))
    0))

(defun gobject-align--arglist-identifier-width (arglist)
  (let ((width 0)
	argument-width)
    (dolist (argument arglist)
      (setq argument-width (gobject-align--argument-identifier-width argument))
      (when (> argument-width width)
	(setq width argument-width)))
    width))

(defun gobject-align--normalize-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\s-*," nil t)
	(replace-match ",\n"))
      (goto-char (point-min))
      (delete-trailing-whitespace)
      ;; Remove whitespace at the beginning of line
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+" nil t)
	(replace-match ""))
      ;; Remove empty lines
      (goto-char (point-min))
      (delete-matching-lines "^$"))))

(defun gobject-align--parse-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (type-start
	    type-end
	    identifier-start
	    identifier-end
	    arglist
	    last-token-start
	    c)
	(goto-char (point-max))
	(while (not (bobp))
	  (c-backward-syntactic-ws)
	  (setq identifier-end (point-marker))
	  ;; Array argument, such as 'int a[]'
	  (if (eq (preceding-char) ?\])
	      (c-backward-sexp))
	  (c-backward-token-2)
	  (setq identifier-start (point-marker))
	  (c-backward-syntactic-ws)
	  (if (or (bobp) (eq (preceding-char) ?,))
	      ;; Identifier is omitted, or '...'.
	      (setq type-start identifier-start
		    type-end identifier-end
		    identifier-start nil
		    identifier-end nil)
	    (setq type-end (point-marker)
		  last-token-start type-end)
	    (while (and (not (bobp))
			(progn
			  (c-backward-token-2)
			  (unless (eq (setq c (char-after)) ?,)
			    (setq last-token-start (point-marker)))))
	      (c-backward-syntactic-ws))
	    (setq type-start last-token-start))
	  (push (gobject-align--make-argument type-start type-end
					      identifier-start identifier-end)
		arglist))
	arglist))))

;;;###autoload
(defun gobject-align-at-point (&optional identifier-start-column)
  "Reformat argument list at point, aligning argument to the right end."
  (interactive)
  (save-excursion
    (let* (start-column arglist)
      (cl-destructuring-bind (beg end)
	  (gobject-align--arglist-region-at-point (point))
	(goto-char beg)
	(setq start-column (current-column))
	(save-restriction
	  (narrow-to-region beg end)
	  (setq arglist (gobject-align--parse-arglist (point-min) (point-max)))
	  (gobject-align--normalize-arglist (point-min) (point-max))
	  (unless identifier-start-column
	    (setq identifier-start-column
		  (gobject-align--arglist-identifier-start-column arglist 0)))
	  (dolist (argument arglist)
	    (goto-char (gobject-align--argument-type-start argument))
	    (let ((column (if (bobp) 0 start-column)))
	      (when (not (bobp))
		(gobject-align--indent-to-column start-column))
	      (when (gobject-align--argument-identifier-start argument)
		(setq column (+ column identifier-start-column))
		(goto-char (gobject-align--argument-identifier-start argument))
		(gobject-align--indent-to-column column)))))))))

(cl-defstruct (gobject-align--decl
	       (:constructor nil)
	       (:constructor gobject-align--make-decl (start
						       end
						       identifier-start
						       identifier-end
						       arglist-start
						       arglist-end
						       arglist))
	       (:copier nil)
	       (:predicate nil))
  (start nil :read-only t)
  (end nil :read-only t)
  (identifier-start nil :read-only t)
  (identifier-end nil :read-only t)
  (arglist-start nil :read-only t)
  (arglist-end nil :read-only t)
  (arglist nil :read-only t))

(defun gobject-align--decls-identifier-start-column (decls start-column)
  (let ((column start-column)
	decl-column)
    (dolist (decl decls)
      (setq decl-column (+ start-column
			   (gobject-align--marker-column
			    (gobject-align--decl-identifier-start decl))))
      (when (and (<= decl-column gobject-align-max-column)
		 (> decl-column column))
	(setq column decl-column)))
    column))

(defun gobject-align--decl-identifier-width (decl)
  (- (gobject-align--marker-column
      (gobject-align--decl-identifier-end decl))
     (gobject-align--marker-column
      (gobject-align--decl-identifier-start decl))))

(defun gobject-align--decls-arglist-start-column (decls start-column)
  (let ((column start-column)
	decl-column
	(arglist-width
	 (+ (gobject-align--decls-arglist-identifier-start-column decls 0)
	    (gobject-align--decls-arglist-identifier-width decls)
	    (length ");"))))
    (dolist (decl decls)
      (setq decl-column (+ start-column
			   (gobject-align--decl-identifier-width decl)))
      (when (and (<= (+ decl-column arglist-width)
		     gobject-align-max-column)
		 (> decl-column column))
	(setq column decl-column)))
    (1+ column)))

(defun gobject-align--decls-arglist-identifier-width (decls)
  (let ((width 0)
	decl-width)
    (dolist (decl decls)
      (setq decl-width (gobject-align--arglist-identifier-width
			(gobject-align--decl-arglist decl)))
      (when (> decl-width width)
	(setq width decl-width)))
    width))

(defun gobject-align--decls-arglist-identifier-start-column (decls start-column)
  (let ((column start-column)
	decl-column)
    (dolist (decl decls)
      (setq decl-column (gobject-align--arglist-identifier-start-column
			 (gobject-align--decl-arglist decl)
			 start-column))
      ;; FIXME: should wrap lines inside argument list?
      (when (> decl-column column)
	(setq column decl-column)))
    column))

(defun gobject-align--parse-decl (beg end)
  ;; Parse at most one func declaration found in BEG END.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (arglist-start
	    arglist-end
	    identifier-start
	    identifier-end)
	(goto-char (point-min))
	(c-forward-syntactic-ws)
	(unless (looking-at
		 "typedef\\|#\\|G_DECLARE_\\(?:\\(?:FINAL\\|DECLARATIVE\\)_TYPE\\|INTERFACE\\)")
	  (while (and (not (eobp))
		      (not (eq (char-after) ?\()))
	    (c-forward-token-2)
	    (c-forward-syntactic-ws))
	  (when (eq (char-after) ?\()
	    (setq arglist-start (point-marker))
	    (c-backward-syntactic-ws)
	    (setq identifier-end (point-marker))
	    (c-backward-token-2)
	    (setq identifier-start (point-marker))
	    (goto-char arglist-start)
	    (c-forward-sexp)
	    (setq arglist-end (point-marker))
	    (gobject-align--make-decl beg end
				      identifier-start identifier-end
				      arglist-start arglist-end
				      (gobject-align--parse-arglist
				       (1+ arglist-start)
				       (1- arglist-end)))))))))

(defun gobject-align--normalize-decl (decl)
  (save-excursion
    (save-restriction
      (narrow-to-region (gobject-align--decl-identifier-start decl)
			(gobject-align--decl-arglist-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match " ")))
    (save-restriction
      (narrow-to-region (gobject-align--decl-start decl)
			(gobject-align--decl-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " ")))))

(defun gobject-align--arglist-region-at-point (point)
  (save-excursion
    (let (start)
      (goto-char point)
      (c-beginning-of-statement-1)
      (c-backward-syntactic-ws)
      (unless (eq ?\( (preceding-char))
	(error "No containing argument list"))
      (setq start (point))
      (backward-char)
      (condition-case nil
	  (c-forward-sexp)
	(error
	 (error "No closing parenthesis")))
      (backward-char)
      (list start (point)))))

;;;###autoload
(defun gobject-align-set-column (symbol)
  "Set alignment column of SYMBOL."
  (interactive
   (let ((symbol-name (completing-read "Symbol to change: "
				       '("identifier-start"
					 "arglist-start"
					 "arglist-identifier-start")
				       nil t)))
     (list (intern (format "gobject-align-%s-column" symbol-name)))))
  (set symbol (current-column)))

(defun gobject-align--scan-decls (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (decls)
	(while (not (eobp))
	  (let (decl-start decl-end decl)
	    (c-forward-syntactic-ws)
	    (setq decl-start (point-marker))
	    (c-end-of-statement)
	    (setq decl-end (point-marker))
	    (setq decl (gobject-align--parse-decl decl-start decl-end))
	    (when decl
	      (push decl decls))))
	decls))))

(defun gobject-align--guess-columns (beg end)
  (let ((buffer (current-buffer))
	decls)
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer beg end)
      (c-mode)
      (setq decls (gobject-align--scan-decls (point-min) (point-max)))
      (mapc #'gobject-align--normalize-decl decls)
      (let* ((identifier-start-column
	      (gobject-align--decls-identifier-start-column
	       decls 0))
	     (arglist-start-column
	      (gobject-align--decls-arglist-start-column
	       decls identifier-start-column))
	     (arglist-identifier-start-column
	      (gobject-align--decls-arglist-identifier-start-column
	       decls (+ (length "(") arglist-start-column))))
	(message
	 "identifier-start: %d, arglist-start: %d, arglist-identifier-start: %d"
	 identifier-start-column
	 arglist-start-column
	 arglist-identifier-start-column)
	(list (cons 'identifier-start-column
		    identifier-start-column)
	      (cons 'arglist-start-column
		    arglist-start-column)
	      (cons 'arglist-identifier-start-column
		    arglist-identifier-start-column))))))

;;;###autoload
(defun gobject-align-guess-columns (beg end)
  "Guess the alignment rule from the function declarations in BEG and END"
  (interactive "r")
  (let ((columns (gobject-align--guess-columns beg end)))
    (setq gobject-align-identifier-start-column
	  (cdr (assq 'identifier-start-column columns))
	  gobject-align-arglist-start-column
	  (cdr (assq 'arglist-start-column columns))
	  gobject-align-arglist-identifier-start-column
	  (cdr (assq 'arglist-identifier-start-column columns)))))

;;;###autoload
(defun gobject-align-region (beg end)
  "Reformat function declarations in the region between BEG and END."
  (interactive "r")
  (save-excursion
    (let (decls)
      (save-restriction
	(narrow-to-region beg end)
	(unless (and gobject-align-identifier-start-column
		     gobject-align-arglist-start-column
		     gobject-align-arglist-identifier-start-column)
	  (let ((columns (gobject-align--guess-columns beg end)))
	    (unless gobject-align-identifier-start-column
	      (setq gobject-align-identifier-start-column
		    (cdr (assq 'identifier-start-column columns))))
	    (unless gobject-align-arglist-start-column
	      (setq gobject-align-arglist-start-column
		    (cdr (assq 'arglist-start-column columns))))
	    (unless gobject-align-arglist-identifier-start-column
	      (setq gobject-align-arglist-identifier-start-column
		    (cdr (assq 'arglist-identifier-start-column columns))))))
	(setq decls (gobject-align--scan-decls beg end))
	(mapc #'gobject-align--normalize-decl decls)
	(dolist (decl decls)
	  (goto-char (gobject-align--decl-identifier-start decl))
	  (gobject-align--indent-to-column
	   gobject-align-identifier-start-column)
	  (goto-char (gobject-align--decl-identifier-end decl))
	  (when (>= (current-column) gobject-align-arglist-start-column)
	    (insert "\n"))
	  (goto-char (gobject-align--decl-arglist-start decl))
	  (gobject-align--indent-to-column
	   gobject-align-arglist-start-column)
	  (forward-char)
	  (gobject-align-at-point
	   (- gobject-align-arglist-identifier-start-column
	      gobject-align-arglist-start-column
	      (length "("))))))))

(provide 'gobject-align)

;;; gobject-align.el ends here
