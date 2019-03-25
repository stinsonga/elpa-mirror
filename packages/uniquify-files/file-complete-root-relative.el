;;; file-complete-root-relative.el --- Completion style for files  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;; Keywords: completion
;; Version: 0
;; package-requires: ((emacs "25.0"))
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary

;; A file completion style in which the root directory is left out of
;; the completion string displayed to the user.
;;
;; Following the Design section in uniquify-files.el, this completion
;; style has the following string formats:
;;
;; - user: file name relative to a root directory
;;
;; - completion table input: same as user
;;
;; - data: absolute file name
;;
;; The completion style requires knowlege of the root directory;
;; currently, this requires use of a completion function to provide a
;; place to store it.

(require 'cl-lib)

(require 'file-complete)

(defun fc-root-rel--root (table)
  "Return root from TABLE."
  (cdr (assoc 'root (completion-metadata "" table nil))))

(defun fc-root-rel-to-table-input (user-string _table _pred)
  "Implement `completion-to-table-input' for file-root-rel."
  user-string)

(defun fc-root-rel-to-data (user-string table _pred)
  "Implement `completion-get-data-string' for file-root-rel."
  ;; We assume USER-STRING is complete and unique.
  (let ((root (fc-root-rel--root table)))
    (concat root user-string)))

(defun fc-root-rel-to-user (data-string-list root)
  "Convert DATA-STRING-LIST to list of user format strings."
  ;; Assume they all start with ROOT, which ends in /
  (let ((prefix-length (length root)))
    (mapcar
     (lambda (abs-file-name)
       (substring abs-file-name prefix-length))
     data-string-list)
  ))

(defun fc-root-rel--pcm-merged-pat (string all point)
  "Return a pcm pattern that is the merged completion of STRING in ALL.
ALL must be a list of table input format strings?
Pattern is in reverse order."
  (let* ((case-fold-search completion-ignore-case)
	 (completion-pcm--delim-wild-regex
	  (concat "[" completion-pcm-word-delimiters "*]"))
	 (pattern (completion-pcm--string->pattern string point)))
    (completion-pcm--merge-completions all pattern)
    ))

(defun fc-root-rel-try-completion (string table pred point)
  "Implement `completion-try-completion' for file-root-rel."
  ;; Returns list of user format strings, nil, or t.
  (let (result
	rel-all
	done)

    (setq completion-current-style 'file-root-rel)

    ;; Compute result, set done.
    (cond
     ((functionp table)
      (setq rel-all (fc-root-rel-all-completions string table pred point))

      (cond
       ((null rel-all) ;; No matches.
	(setq result nil)
	(setq done t))

       ((= 1 (length rel-all)) ;; One match; unique.
	(setq done t)

	;; Check for valid completion
	(if (string-equal string (car rel-all))
	    (setq result t)

	  (setq result (car rel-all))
	  (setq result (cons result (length result)))))

       (t ;; Multiple matches
	(setq done nil))
       ))

     ;; The following cases handle being called from
     ;; icomplete-completions with the result of `all-completions'
     ;; instead of the real table function. TABLE is a list of
     ;; relative file names.

     ((null table) ;; No matches.
      (setq result nil)
      (setq done t))

     (t
      (setq rel-all table)
      (setq done nil))
     )

    (if done
	result

      ;; Find merged completion of relative file names
      (let* ((merged-pat (fc-root-rel--pcm-merged-pat string rel-all point))

	     ;; `merged-pat' is in reverse order.  Place new point at:
	     (point-pat (or (memq 'point merged-pat) ;; the old point
			    (memq 'any   merged-pat) ;; a place where there's something to choose
			    (memq 'star  merged-pat) ;; ""
			    merged-pat))             ;; the end

	     ;; `merged-pat' does not contain 'point when the field
	     ;; containing 'point is fully completed.

	     (new-point (length (completion-pcm--pattern->string point-pat)))

	     ;; Compute this after `new-point' because `nreverse'
	     ;; changes `point-pat' by side effect.
	     (merged (completion-pcm--pattern->string (nreverse merged-pat))))

	(cons merged new-point)))
   ))

(defun fc-root-rel--hilit (string all point)
  "Apply face text properties to each element of ALL.
STRING is the current user input.
ALL is a list of strings in user format.
POINT is the position of point in STRING.
Returns new list.

Adds the face `completions-first-difference' to the first
character after each completion field."
  (let* ((merged-pat (nreverse (fc-root-rel--pcm-merged-pat string all point)))
	 (field-count 0)
	 (regex (completion-pcm--pattern->regex merged-pat '(any star any-delim point)))
	 )
    (dolist (x merged-pat)
      (when (not (stringp x))
	(setq field-count (1+ field-count))))

    (mapcar
     (lambda (str)
       (when (string-match regex str)
	 (cl-loop
	  for i from 1 to field-count
	  do
	  (when (and
		 (match-beginning i)
		 (<= (1+ (match-beginning i)) (length str)))
	    (put-text-property (match-beginning i) (1+ (match-beginning i)) 'face 'completions-first-difference str))
	  ))
       str)
     all)))

(defun fc-root-rel-all-completions (user-string table pred point)
  "Implement `completion-all-completions' for root-relative."
  ;; Returns list of data format strings (abs file names).

  (setq completion-current-style 'file-root-rel)

  ;; Note that we never get here with TABLE a list of filenames.
  (let* ((table-string (fc-root-rel-to-table-input user-string table pred))
	 (all (funcall table table-string pred t)))

    (when all
      (setq all (fc-root-rel-to-user all (fc-root-rel--root table)))
      (setq all (fc-root-rel--hilit user-string all point))
      all
      )))

(defun fc-root-rel-completion-table-iter (path-iter string pred action)
  "Implement a completion table for file names in PATH-ITER.

PATH-ITER is a `path-iterator' object; it must have exacly one
recursive root, and no non-recursive roots.

STRING, PRED, ACTION are completion table arguments."

  (let ((root (car (path-iter-path-recursive-init path-iter))))
    (cond
     ((eq action 'metadata)
      (cons 'metadata
	    (list
	     '(category . project-file)
	     '(styles . (file-root-rel))
	     (cons 'root root))))

     (t
      (file-complete-completion-table path-iter 'root-relative root string pred action))
     )))

(defun fc-root-rel--pcm-regex-list (string root)
  "Return pcm regex constructed from STRING (a table format string)."
  (let ((pattern (completion-pcm--string->pattern string)))
    (concat "\\`"
	    root
	    (substring (completion-pcm--pattern->regex pattern) 2);; trim \`
	    )))

(defun fc-root-rel-completion-table-list (file-list root string pred action)
  "Implement a completion table for file names in FILE-LIST,
with common prefix ROOT.

STRING, PRED, ACTION are completion table arguments."

  ;; This completion table function is required to provide access to
  ;; ROOT via metadata, and the file-root-rel suggested style.

  ;; `completion-to-table-input' doesn't realize we are dealing with a
  ;; list, so we have to convert to abs file name.
  (setq root (file-name-as-directory root))
  (let ((abs-name (concat (file-name-as-directory root) string)))

    (cond
     ((eq (car-safe action) 'boundaries)
      ;; We don't use boundaries; return the default definition.
      (cons 'boundaries
	    (cons 0 (length (cdr action)))))

     ((eq action 'metadata)
      (cons 'metadata
	       (list
		'(category . project-file)
		'(styles . (file-root-rel))
		(cons 'root (file-name-as-directory root)))))

     ((memq action
	    '(nil    ;; Called from `try-completion'
	      lambda ;; Called from `test-completion'
	      t))    ;; Called from all-completions

      (let ((regex (fc-root-rel--pcm-regex-list string root))
	    (case-fold-search completion-ignore-case)
	    (result nil))
	(dolist (abs-file-name file-list)
	  (when (and
		 (string-match regex abs-file-name)
		 (or (null pred)
		     (funcall pred abs-file-name)))
	    (push abs-file-name result)))

	(cond
	 ((null action)
	  (try-completion abs-name result))

	 ((eq 'lambda action)
	  (test-completion abs-name file-list pred))

	 ((eq t action)
	  result)
	 )))
     )))

(add-to-list 'completion-styles-alist
	     '(file-root-rel
	       fc-root-rel-try-completion
	       fc-root-rel-all-completions
	       "root relative hierarchical filenames."
	       fc-root-rel-to-table-input    ;; 4 user to table input format
	       fc-root-rel-to-data)) ;; 5 user to data format

(defun locate-root-rel-file-iter (iter &optional predicate default prompt)
  "Return an absolute filename, with file-root-rel completion style.
ITER is a path-iterator giving the directory path to search; it
must have exacly one recursive root, and no non-recursive roots.
If PREDICATE is nil, it is ignored. If non-nil, it must be a
function that takes one argument; the absolute file name.  The
file name is included in the result if PRED returns
non-nil. DEFAULT is the default for completion.

In the user input string, `*' is treated as a wildcard."
  (let* ((table (apply-partially #'fc-root-rel-completion-table-iter iter))
	 (table-styles (cdr (assq 'styles (completion-metadata "" table nil))))
	 (completion-category-overrides
	  (list (list 'project-file (cons 'styles table-styles)))))

    (unless (and (= 0 (length (path-iter-path-non-recursive-init iter)))
		 (= 1 (length (path-iter-path-recursive-init iter))))
      (user-error "iterator does not have exactly one recursive root"))

    (completing-read (format (concat (or prompt "file") " (%s): ") default)
		     table
		     predicate t nil nil default)
    ))

;; For example:
;; (locate-root-rel-file-iter
;;  (make-path-iterator
;;   :user-path-non-recursive nil
;;   :user-path-recursive "c:/Projects/elpa/packages/uniquify-files/uniquify-files-resources"))

(provide 'file-complete-root-relative)
;;; file-complete-root-relative.el ends here
