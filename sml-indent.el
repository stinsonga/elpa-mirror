;;; sml-indent.el --- Old indentation code for SML-mode

;; Copyright (C) 2010  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: 

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

;;; Code:

(require 'sml-move)


(defun sml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (max (or (ignore-errors (sml-calculate-indentation)) 0) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sml-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (sml-with-ist
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line.
     (when (looking-at "(\\*") (sml-forward-spaces))
     (let (data
	   (sym (save-excursion (sml-forward-sym))))
       (or
	;; Allow the user to override the indentation.
	(when (looking-at (concat ".*" (regexp-quote comment-start)
				  "[ \t]*fixindent[ \t]*"
				  (regexp-quote comment-end)))
	  (current-indentation))

	;; Continued comment.
	(and (looking-at "\\*") (sml-find-comment-indent))

	;; Continued string ? (Added 890113 lbn)
	(and (looking-at "\\\\")
             (or (save-excursion (forward-line -1)
                                 (if (looking-at "[\t ]*\\\\")
                                     (current-indentation)))
                 (save-excursion
                   (if (re-search-backward "[^\\\\]\"" nil t)
                       (1+ (current-column))
                     0))))

	;; Closing parens.  Could be handled below with `sml-indent-relative'?
	(and (looking-at "\\s)")
	     (save-excursion
	       (skip-syntax-forward ")")
	       (backward-sexp 1)
	       (if (sml-dangling-sym)
		   (sml-indent-default 'noindent)
		 (current-column))))

	(and (setq data (assoc sym sml-close-paren))
	     (sml-indent-relative sym data))

	(and (member sym sml-starters-syms)
	     (sml-indent-starter sym))

	(and (string= sym "|") (sml-indent-pipe))

	(sml-indent-arg)
	(sml-indent-default))))))

(defun sml-find-comment-indent ()
  (save-excursion
    (let ((depth 1))
      (while (> depth 0)
	(if (re-search-backward "(\\*\\|\\*)" nil t)
	    (cond
	     ;; FIXME: That's just a stop-gap.
	     ((eq (get-text-property (point) 'face) 'font-lock-string-face))
	     ((looking-at "*)") (incf depth))
	     ((looking-at comment-start-skip) (decf depth)))
	  (setq depth -1)))
      (if (= depth 0)
	  (1+ (current-column))
	nil))))

(defsubst sml-bolp ()
  (save-excursion (skip-chars-backward " \t|") (bolp)))

(defun sml-first-starter-p ()
  "Non-nil if starter at point is immediately preceded by let/local/in/..."
  (save-excursion
    (let ((sym (unless (save-excursion (sml-backward-arg))
                 (sml-backward-spaces)
                 (sml-backward-sym))))
      (if (member sym '(";" "d=")) (setq sym nil))
      sym)))


(defun sml-indent-starter (orig-sym)
  "Return the indentation to use for a symbol in `sml-starters-syms'.
Point should be just before the symbol ORIG-SYM and is not preserved."
  (let ((sym (unless (save-excursion (sml-backward-arg))
	       (sml-backward-spaces)
	       (sml-backward-sym))))
    (if (member sym '(";" "d=")) (setq sym nil))
    (if sym (sml-get-sym-indent sym)
      ;; FIXME: this can take a *long* time !!
      (setq sym (sml-find-matching-starter sml-starters-syms))
      (if (or (sml-first-starter-p)
              ;; Don't align with `and' because it might be specially indented.
              (and (or (equal orig-sym "and") (not (equal sym "and")))
                   (sml-bolp)))
	  (+ (current-column)
	     (if (and sml-rightalign-and (equal orig-sym "and"))
		 (- (length sym) 3) 0))
	(sml-indent-starter orig-sym)))))

(defun sml-indent-relative (sym data)
  (save-excursion
    (sml-forward-sym) (sml-backward-sexp nil)
    (unless (second data) (sml-backward-spaces) (sml-backward-sym))
    (+ (or (cdr (assoc sym sml-symbol-indent)) 0)
       (sml-delegated-indent))))

(defun sml-indent-pipe ()
  (let ((sym (sml-find-matching-starter sml-pipeheads
					(sml-op-prec "|" 'back))))
    (when sym
      (if (string= sym "|")
	  (if (sml-bolp) (current-column) (sml-indent-pipe))
	(let ((pipe-indent (or (cdr (assoc "|" sml-symbol-indent)) -2)))
	  (when (or (member sym '("datatype" "abstype"))
		    (and (equal sym "and")
			 (save-excursion
			   (forward-word 1)
			   (not (sml-funname-of-and)))))
	    (re-search-forward "="))
	  (sml-forward-sym)
	  (sml-forward-spaces)
	  (+ pipe-indent (current-column)))))))

(defun sml-find-forward (re)
  (sml-forward-spaces)
  (while (and (not (looking-at re))
	      (progn
		(or (ignore-errors (forward-sexp 1) t) (forward-char 1))
		(sml-forward-spaces)
		(not (looking-at re))))))

(defun sml-indent-arg ()
  (and (save-excursion (ignore-errors (sml-forward-arg)))
       ;;(not (looking-at sml-not-arg-re))
       ;; looks like a function or an argument
       (sml-move-if (sml-backward-arg))
       ;; an argument
       (if (save-excursion (not (sml-backward-arg)))
	   ;; a first argument
	   (+ (current-column) sml-indent-args)
	 ;; not a first arg
	 (while (and (/= (current-column) (current-indentation))
		     (sml-move-if (sml-backward-arg))))
	 (unless (save-excursion (sml-backward-arg))
	   ;; all earlier args are on the same line
	   (sml-forward-arg) (sml-forward-spaces))
	 (current-column))))

(defun sml-get-indent (data sym)
  (let (d)
    (cond
     ((not (listp data)) data)
     ((setq d (member sym data)) (cadr d))
     ((and (consp data) (not (stringp (car data)))) (car data))
     (t sml-indent-level))))

(defun sml-dangling-sym ()
  "Non-nil if the symbol after point is dangling.
The symbol can be an SML symbol or an open-paren. \"Dangling\" means that
it is not on its own line but is the last element on that line."
  (save-excursion
    (and (not (sml-bolp))
	 (< (sml-point-after (end-of-line))
	    (sml-point-after (or (sml-forward-sym) (skip-syntax-forward "("))
			     (sml-forward-spaces))))))

(defun sml-delegated-indent ()
  (if (sml-dangling-sym)
      (sml-indent-default 'noindent)
    (sml-move-if (backward-word 1)
		 (looking-at sml-agglomerate-re))
    (current-column)))

(defun sml-get-sym-indent (sym &optional style)
  "Find the indentation for the SYM we're `looking-at'.
If indentation is delegated, point will move to the start of the parent.
Optional argument STYLE is currently ignored."
  (assert (equal sym (save-excursion (sml-forward-sym))))
  (save-excursion
    (let ((delegate (and (not (equal sym "end")) (assoc sym sml-close-paren)))
	  (head-sym sym))
      (when (and delegate (not (eval (third delegate))))
	;;(sml-find-match-backward sym delegate)
	(sml-forward-sym) (sml-backward-sexp nil)
	(setq head-sym
	      (if (second delegate)
		  (save-excursion (sml-forward-sym))
		(sml-backward-spaces) (sml-backward-sym))))

      (let ((idata (assoc head-sym sml-indent-rule)))
	(when idata
	  ;;(if (or style (not delegate))
	  ;; normal indentation
	  (let ((indent (sml-get-indent (cdr idata) sym)))
	    (when indent (+ (sml-delegated-indent) indent)))
	  ;; delgate indentation to the parent
	  ;;(sml-forward-sym) (sml-backward-sexp nil)
	  ;;(let* ((parent-sym (save-excursion (sml-forward-sym)))
	  ;;     (parent-indent (cdr (assoc parent-sym sml-indent-starters))))
	  ;; check the special rules
	  ;;(+ (sml-delegated-indent)
	  ;; (or (sml-get-indent (cdr indent-data) 1 'strict)
	  ;; (sml-get-indent (cdr parent-indent) 1 'strict)
	  ;; (sml-get-indent (cdr indent-data) 0)
	  ;; (sml-get-indent (cdr parent-indent) 0))))))))
	  )))))

(defun sml-indent-default (&optional noindent)
  (let* ((sym-after (save-excursion (sml-forward-sym)))
	 (_ (sml-backward-spaces))
	 (sym-before (sml-backward-sym))
	 (sym-indent (and sym-before (sml-get-sym-indent sym-before)))
	 (indent-after (or (cdr (assoc sym-after sml-symbol-indent)) 0)))
    (when (equal sym-before "end")
      ;; I don't understand what's really happening here, but when
      ;; it's `end' clearly, we need to do something special.
      (forward-word 1)
      (setq sym-before nil sym-indent nil))
    (cond
     (sym-indent
      ;; the previous sym is an indentation introducer: follow the rule
      (if noindent
	  ;;(current-column)
	  sym-indent
	(+ sym-indent indent-after)))
     ;; If we're just after a hanging open paren.
     ((and (eq (char-syntax (preceding-char)) ?\()
	   (save-excursion (backward-char) (sml-dangling-sym)))
      (backward-char)
      (sml-indent-default))
     (t
      ;; default-default
      (let* ((prec-after (sml-op-prec sym-after 'back))
	     (prec (or (sml-op-prec sym-before 'back) prec-after 100)))
	;; go back until you hit a symbol that has a lower prec than the
	;; "current one", or until you backed over a sym that has the same prec
	;; but is at the beginning of a line.
	(while (and (not (sml-bolp))
		    (while (sml-move-if (sml-backward-sexp (1- prec))))
		    (not (sml-bolp)))
	  (while (sml-move-if (sml-backward-sexp prec))))
	(if noindent
	    ;; the `noindent' case does back over an introductory symbol
	    ;; such as `fun', ...
	    (progn
	      (sml-move-if
	       (sml-backward-spaces)
	       (member (sml-backward-sym) sml-starters-syms))
	      (current-column))
	  ;; Use `indent-after' for cases such as when , or ; should be
	  ;; outdented so that their following terms are aligned.
	  (+ (if (progn
		   (if (equal sym-after ";")
		       (sml-move-if
			(sml-backward-spaces)
			(member (sml-backward-sym) sml-starters-syms)))
		   (and sym-after (not (looking-at sym-after))))
		 indent-after 0)
	     (current-column))))))))


;; maybe `|' should be set to word-syntax in our temp syntax table ?
(defun sml-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t|")
    (current-column)))


(defun sml-find-matching-starter (syms &optional prec)
  (let (sym)
    (ignore-errors
      (while
	  (progn (sml-backward-sexp prec)
		 (setq sym (save-excursion (sml-forward-sym)))
		 (not (or (member sym syms) (bobp)))))
      (if (member sym syms) sym))))

(provide 'sml-indent)
;;; sml-indent.el ends here
