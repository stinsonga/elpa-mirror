;; gnome-align.el --- GNOME-style code alignment -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Daiki Ueno <ueno@gnu.org>

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style

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

(defgroup gnome-minor-mode nil
  "GNOME-style C source code editing"
  :prefix "gnome-"
  :group 'c)

(defcustom gnome-align-max-column 80
  "Maximum number of columns per line."
  :type '(choice (integer :tag "Columns")
		 (const :tag "No wrap"))
  :group 'gnome-minor-mode)

(defvar gnome-align-identifier-start-column nil)
(make-variable-buffer-local 'gnome-align-identifier-start-column)

(defvar gnome-align-arglist-start-column nil)
(make-variable-buffer-local 'gnome-align-arglist-start-column)

(defvar gnome-align-arglist-identifier-start-column nil)
(make-variable-buffer-local 'gnome-align-arglist-identifier-start-column)

(cl-defstruct (gnome-align--argument
	       (:constructor nil)
	       (:constructor gnome-align--make-argument (type-start
							 type-identifier-end
							 type-end
							 identifier-start
							 identifier-end))
	       (:copier nil)
	       (:predicate nil))
  (type-start nil :read-only t)
  (type-identifier-end nil :read-only t)
  (type-end nil :read-only t)
  (identifier-start nil :read-only t)
  (identifier-end nil :read-only t))

(defun gnome-align--marker-column (marker)
  (save-excursion
    (goto-char marker)
    (current-column)))

(defun gnome-align--indent-to-column (column)
  ;; Prefer 'char **foo' than 'char ** foo'
  (when (looking-back "\*+" nil t)
    (setq column (- column (- (match-end 0) (match-beginning 0))))
    (goto-char (match-beginning 0)))
  ;; FIXME: should respect indent-tabs-mode?
  (let (indent-tabs-mode)
    (indent-to-column column)))

(defun gnome-align--argument-type-width (arg)
  (- (gnome-align--marker-column (gnome-align--argument-type-end arg))
     (gnome-align--marker-column (gnome-align--argument-type-start arg))))

(defun gnome-align--argument-type-identifier-width (arg)
  (- (gnome-align--marker-column
      (gnome-align--argument-type-identifier-end arg))
     (gnome-align--marker-column
      (gnome-align--argument-type-start arg))))

(defun gnome-align--arglist-identifier-start-column (arglist start-column)
  (let ((max-type-identifier-width
	 (apply #'max
		(mapcar #'gnome-align--argument-type-identifier-width arglist)))
	(max-extra-width
	 (apply #'max
		(mapcar
		 (lambda (argument)
		   (- (gnome-align--argument-type-end argument)
		      (gnome-align--argument-type-identifier-end argument)))
		 arglist))))
    (+ start-column max-type-identifier-width max-extra-width)))

(defun gnome-align--argument-identifier-width (argument)
  (if (gnome-align--argument-identifier-start argument)
      (- (gnome-align--marker-column
	  (gnome-align--argument-identifier-end argument))
	 (gnome-align--marker-column
	  (gnome-align--argument-identifier-start argument)))
    0))

(defun gnome-align--arglist-identifier-width (arglist)
  (apply #'max (mapcar #'gnome-align--argument-identifier-width arglist)))

(defun gnome-align--normalize-arglist-region (beg end)
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

(defun gnome-align--parse-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (type-start
	    type-identifier-end
	    type-end
	    identifier-start
	    identifier-end
	    arglist
	    last-token-start)
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
	      (progn
		;; Identifier is omitted, or '...'.
		(setq type-start identifier-start
		      type-identifier-end identifier-end
		      type-end identifier-end
		      identifier-start nil
		      identifier-end nil)
		(c-backward-token-2))
	    (setq type-end (point-marker)
		  last-token-start type-end)
	    (while (and (not (bobp))
			(progn
			  (c-backward-token-2)
			  (unless (eq (char-after) ?,)
			    (setq last-token-start (point-marker)))))
	      (c-backward-syntactic-ws))
	    (setq type-start last-token-start)
	    (save-excursion
	      (goto-char type-end)
	      (skip-chars-backward "*" type-start)
	      (c-backward-syntactic-ws)
	      (setq type-identifier-end (point-marker))))
	  (push (gnome-align--make-argument type-start
					    type-identifier-end
					    type-end
					    identifier-start
					    identifier-end)
		arglist))
	arglist))))

;;;###autoload
(defun gnome-align-at-point (&optional identifier-start-column)
  "Reformat argument list at point, aligning argument to the right end."
  (interactive)
  (save-excursion
    (let* (start-column arglist)
      (cl-destructuring-bind (beg end)
	  (gnome-align--arglist-region-at-point (point))
	(goto-char beg)
	(setq start-column (current-column))
	(save-restriction
	  (narrow-to-region beg end)
	  (setq arglist (gnome-align--parse-arglist (point-min) (point-max)))
	  (gnome-align--normalize-arglist-region (point-min) (point-max))
	  (unless identifier-start-column
	    (setq identifier-start-column
		  (gnome-align--arglist-identifier-start-column arglist 0)))
	  (dolist (argument arglist)
	    (goto-char (gnome-align--argument-type-start argument))
	    (let ((column (if (bobp) 0 start-column)))
	      (when (not (bobp))
		(gnome-align--indent-to-column start-column))
	      (when (gnome-align--argument-identifier-start argument)
		(setq column (+ column identifier-start-column))
		(goto-char (gnome-align--argument-identifier-start argument))
		(gnome-align--indent-to-column column)))))))))

(cl-defstruct (gnome-align--decl
	       (:constructor nil)
	       (:constructor gnome-align--make-decl (start
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

(defun gnome-align--decls-identifier-start-column (decls start-column)
  (apply #'max
	 (delq nil
	       (mapcar
		(lambda (decl)
		  (let ((decl-column
			 (+ start-column
			    (gnome-align--marker-column
			     (gnome-align--decl-identifier-start decl)))))
		    (if (and gnome-align-max-column
			     (> decl-column gnome-align-max-column))
			nil
		      decl-column)))
		decls))))

(defun gnome-align--decl-identifier-width (decl)
  (- (gnome-align--marker-column
      (gnome-align--decl-identifier-end decl))
     (gnome-align--marker-column
      (gnome-align--decl-identifier-start decl))))

(defun gnome-align--decls-arglist-start-column (decls start-column)
  (let ((arglist-width
	 (+ (gnome-align--decls-arglist-identifier-start-column decls 0)
	    (gnome-align--decls-arglist-identifier-width decls)
	    (length ");"))))
    (apply #'max
	   (delq nil
		 (mapcar
		  (lambda (decl)
		    (let ((decl-column
			   (+ start-column
			      (gnome-align--decl-identifier-width decl)
			      1)))
		      (if (and gnome-align-max-column
			       (> (+ decl-column arglist-width)
				  gnome-align-max-column))
			  nil
			decl-column)))
		  decls)))))

(defun gnome-align--decls-arglist-identifier-width (decls)
  (apply #'max (mapcar (lambda (decl)
			 (gnome-align--arglist-identifier-width
			  (gnome-align--decl-arglist decl)))
		       decls)))

(defun gnome-align--decls-arglist-identifier-start-column (decls start-column)
  (apply #'max (mapcar (lambda (decl)
			 ;; FIXME: should wrap lines inside argument list?
			 (gnome-align--arglist-identifier-start-column
			  (gnome-align--decl-arglist decl)
			  start-column))
		       decls)))

(defun gnome-align--parse-decl (beg end)
  ;; Parse at most one func declaration found in BEG END.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (arglist-start
	    arglist-end
	    identifier-start
	    identifier-end
	    vfunc-p)
	(goto-char (point-min))
	(c-forward-syntactic-ws)
	(unless (looking-at
		 "typedef\\|#\\|G_\\(?:DECLARE\\|DEFINE\\)")
	  (while (and (not (eobp))
		      (not (eq (char-after) ?\()))
	    (c-forward-token-2)
	    (c-forward-syntactic-ws))
	  ;; Identifier is vfunc.
	  (when (looking-at "(\\s-*\\*")
	    (c-forward-sexp)
	    (c-forward-syntactic-ws)
	    (setq vfunc-p t))
	  (when (eq (char-after) ?\()
	    (setq arglist-start (point-marker))
	    (c-backward-syntactic-ws)
	    (setq identifier-end (point-marker))
	    (if vfunc-p
		(c-backward-sexp)
	      (c-backward-token-2))
	    (setq identifier-start (point-marker))
	    (goto-char arglist-start)
	    (c-forward-sexp)
	    (setq arglist-end (point-marker))
	    (gnome-align--make-decl beg end
				      identifier-start identifier-end
				      arglist-start arglist-end
				      (gnome-align--parse-arglist
				       (1+ arglist-start)
				       (1- arglist-end)))))))))

(defun gnome-align--normalize-decl (decl)
  (save-excursion
    (save-restriction
      (narrow-to-region (gnome-align--decl-identifier-start decl)
			(gnome-align--decl-arglist-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match " ")))
    (save-restriction
      (narrow-to-region (gnome-align--decl-start decl)
			(gnome-align--decl-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " ")))))

(defun gnome-align--arglist-region-at-point (point)
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
(defun gnome-align-set-column (symbol)
  "Set alignment column of SYMBOL."
  (interactive
   (let ((symbol-name (completing-read "Symbol to change: "
				       '("identifier-start"
					 "arglist-start"
					 "arglist-identifier-start")
				       nil t)))
     (list (intern (format "gnome-align-%s-column" symbol-name)))))
  (set symbol (current-column)))

(defun gnome-align--scan-decls (beg end)
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
	    (setq decl (gnome-align--parse-decl decl-start decl-end))
	    (when decl
	      (push decl decls))))
	decls))))

(defun gnome-align--compute-optimal-columns (beg end)
  (let ((buffer (current-buffer))
	decls)
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer beg end)
      (c-mode)
      (setq decls (gnome-align--scan-decls (point-min) (point-max)))
      (mapc #'gnome-align--normalize-decl decls)
      (let* ((identifier-start-column
	      (gnome-align--decls-identifier-start-column
	       decls 0))
	     (arglist-start-column
	      (gnome-align--decls-arglist-start-column
	       decls identifier-start-column))
	     (arglist-identifier-start-column
	      (gnome-align--decls-arglist-identifier-start-column
	       decls (+ (length "(") arglist-start-column))))
	(list (cons 'identifier-start-column
		    identifier-start-column)
	      (cons 'arglist-start-column
		    arglist-start-column)
	      (cons 'arglist-identifier-start-column
		    arglist-identifier-start-column))))))

;;;###autoload
(defun gnome-align-compute-optimal-columns (beg end)
  "Compute the optimal alignment rule from the declarations in BEG and END.

This sets `gnome-align-identifier-start-column',
`gnome-align-arglist-start-column', and
`gnome-align-arglist-identifier-start-column'."
  (interactive "r")
  (let ((columns (gnome-align--compute-optimal-columns beg end)))
    (setq gnome-align-identifier-start-column
	  (cdr (assq 'identifier-start-column columns))
	  gnome-align-arglist-start-column
	  (cdr (assq 'arglist-start-column columns))
	  gnome-align-arglist-identifier-start-column
	  (cdr (assq 'arglist-identifier-start-column columns)))
    (message
     "identifier-start: %d, arglist-start: %d, arglist-identifier-start: %d"
     gnome-align-identifier-start-column
     gnome-align-arglist-start-column
     gnome-align-arglist-identifier-start-column)))

;;;###autoload
(defun gnome-align-guess-columns (beg end)
  "Guess the existing alignment rule from the declarations in BEG and END.

This sets `gnome-align-identifier-start-column',
`gnome-align-arglist-start-column', and
`gnome-align-arglist-identifier-start-column'."
  (interactive "r")
  (let ((decls (gnome-align--scan-decls beg end))
	arglist)
    (unless decls
      (error "No function declaration in the region"))
    (setq arglist (gnome-align--parse-arglist
		   (1+ (gnome-align--decl-arglist-start (car decls)))
		   (1- (gnome-align--decl-arglist-end (car decls)))))
    (unless arglist
      (error "Empty argument list"))
    (unless (gnome-align--argument-identifier-start (car arglist))
      (error "No identifier in the argument list"))
    (setq gnome-align-identifier-start-column
	  (gnome-align--marker-column
	   (gnome-align--decl-identifier-start (car decls)))
	  gnome-align-arglist-start-column
	  (gnome-align--marker-column
	   (gnome-align--decl-arglist-start (car decls)))
	  gnome-align-arglist-identifier-start-column
	  (gnome-align--marker-column
	   (gnome-align--argument-identifier-start (car arglist))))
    (message
     "identifier-start: %d, arglist-start: %d, arglist-identifier-start: %d"
     gnome-align-identifier-start-column
     gnome-align-arglist-start-column
     gnome-align-arglist-identifier-start-column)))

;;;###autoload
(defun gnome-align-region (beg end)
  "Reformat function declarations in the region between BEG and END."
  (interactive "r")
  (save-excursion
    (let (decls)
      (save-restriction
	(narrow-to-region beg end)
	(unless (and gnome-align-identifier-start-column
		     gnome-align-arglist-start-column
		     gnome-align-arglist-identifier-start-column)
	  (let ((columns (gnome-align--compute-optimal-columns beg end)))
	    (unless gnome-align-identifier-start-column
	      (setq gnome-align-identifier-start-column
		    (cdr (assq 'identifier-start-column columns))))
	    (unless gnome-align-arglist-start-column
	      (setq gnome-align-arglist-start-column
		    (cdr (assq 'arglist-start-column columns))))
	    (unless gnome-align-arglist-identifier-start-column
	      (setq gnome-align-arglist-identifier-start-column
		    (cdr (assq 'arglist-identifier-start-column columns))))))
	(setq decls (gnome-align--scan-decls beg end))
	(mapc #'gnome-align--normalize-decl decls)
	(dolist (decl decls)
	  (goto-char (gnome-align--decl-identifier-start decl))
	  (gnome-align--indent-to-column
	   gnome-align-identifier-start-column)
	  (goto-char (gnome-align--decl-identifier-end decl))
	  (when (>= (current-column) gnome-align-arglist-start-column)
	    (insert "\n"))
	  (goto-char (gnome-align--decl-arglist-start decl))
	  (gnome-align--indent-to-column
	   gnome-align-arglist-start-column)
	  (forward-char)
	  (gnome-align-at-point
	   (- (- gnome-align-arglist-identifier-start-column
		 (length "("))
	      gnome-align-arglist-start-column)))))))

(provide 'gnome-align)

;;; gnome-align.el ends here
