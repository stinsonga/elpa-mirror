;;; ada-fix-error.el --- utilities for automatically fixing
;; errors reported by the compiler.

;; Copyright (C) 1999-2009, 2012-2014 Free Software Foundation, Inc.

;; Author     : Stephen Leake      <Stephen_Leake@stephe-leake.org>
;; Maintainer : Stephen Leake      <Stephen_Leake@stephe-leake.org>
;; Web site   : http://www.stephe-leake.org/
;; Keywords   : languages ada error

;; This file is part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;;; code

(require 'ada-mode)
(require 'cl-lib)
(require 'compile)

(defcustom ada-fix-sort-context-clause t
  "*If non-nil, sort context clause when inserting 'with'"
  :type 'boolean
  :group 'ada)

(defvar ada-fix-context-clause nil
  "Function to return the region containing the context clause for the current buffer.
Called with no arguments; return (BEGIN . END). BEGIN and
END must be at beginning of line.  If there is no context
clause, BEGIN = END, at start of compilation unit.")

(defun ada-fix-context-clause ()
  (when ada-fix-context-clause
    (funcall ada-fix-context-clause)))

(defun ada-fix-insert-unit-name (unit-name)
  "Insert UNIT-NAME at point and capitalize it."
  ;; unit-name is normally gotten from a file-name, and is thus all lower-case.
  (let ((start-point (point))
        search-bound)
    (insert unit-name)
    (setq search-bound (point))
    (insert " ") ; separate from following words, if any, for ada-case-adjust-identifier
    (goto-char start-point)
    (while (search-forward "." search-bound t)
      (forward-char -1)
      (ada-case-adjust-identifier)
      (forward-char 1))
    (goto-char search-bound)
    (ada-case-adjust-identifier)
    (delete-char 1)))

(defun ada-fix-add-with-clause (package-name)
  "Add a with_clause for PACKAGE_NAME.
If ada-fix-sort-context-clause, sort the context clauses using
sort-lines."
  (let ((context-clause (ada-fix-context-clause)))
    (when (not context-clause)
      (error "no compilation unit found"))

    (goto-char (cdr context-clause))
    (insert "with ")
    (ada-fix-insert-unit-name package-name)
    (insert ";\n")

    (when (and (< (car context-clause) (cdr context-clause))
	       ada-fix-sort-context-clause)
      ;; FIXME (later): this puts "limited with", "private with" at top of list; prefer at bottom
      (sort-lines nil (car context-clause) (point)))
    ))

(defun ada-fix-extend-with-clause (child-name)
  "Assuming point is in a selected name, just before CHILD-NAME, add or
extend a with_clause to include CHILD-NAME  .	"
  (let ((parent-name-end (point)))
    ;; Find the full parent name; skip back to whitespace, then match
    ;; the name forward.
    (skip-syntax-backward "w_.")
    (search-forward-regexp ada-name-regexp parent-name-end)
    (let ((parent-name (match-string 0))
	  (context-clause (ada-fix-context-clause)))
      (goto-char (car context-clause))
      (if (search-forward-regexp (concat "^with " parent-name ";") (cdr context-clause) t)
	  ;; found exisiting 'with' for parent; extend it
	  (progn
	    (forward-char -1) ; skip back over semicolon
	    (insert "." child-name))

	;; not found; we are in a package body, with_clause for parent is in spec.
	;; insert a new one
	(ada-fix-add-with-clause (concat parent-name "." child-name)))
      )))

(defun ada-fix-add-use-type (type)
  "Insert 'use type' clause for TYPE at start of declarative part for current construct."
  (ada-goto-declarative-region-start); leaves point after 'is'
  (newline)
  (insert "use type " type ";")
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun ada-fix-add-use (package)
  "Insert 'use' clause for PACKAGE at start of declarative part for current construct."
  (ada-goto-declarative-region-start); leaves point after 'is'
  (newline)
  (insert "use " package ";")
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defvar ada-fix-error-hook nil
  ;; determined by ada_compiler, set by *-select-prj-compiler
  "Hook to recognize and fix errors.
Hook functions are called with three args:

MSG, the `compilation--message' struct for the current error

SOURCE-BUFFER, the buffer containing the source to be fixed

SOURCE-WINDOW, the window displaying SOURCE-BUFFER.

Point in SOURCE-BUFFER is at error location; point in
`compilation-last-buffer' is at MSG location. Focus is in
compilation buffer.

Hook functions should return t if the error is recognized and
fixed, leaving point at fix. Otherwise, they should preserve
point and return nil.")

(defun ada-get-compilation-message ()
  "Get compilation message at point.
Compatible with Emacs 23.4 and 24.x."
  (cl-case emacs-major-version
    (23 (get-text-property (line-beginning-position) 'message))
    (24 (get-text-property (line-beginning-position) 'compilation-message))))

(defun ada-fix-compiler-error ()
  "Attempt to fix the current compiler error. Leave point at fixed code."
  (interactive)

  (let ((source-buffer (current-buffer))
        (source-window (selected-window))
        (line-move-visual nil)); screws up next-line otherwise

    (with-current-buffer compilation-last-buffer
      (when (not (ada-get-compilation-message))
	;; not clear why this can happens, but it does
	(compilation-next-error 1))
      (let ((comp-buf-pt (point))
	    (success
	     (run-hook-with-args-until-success
	      ada-fix-error-hook
	      (compilation-next-error 0)
	      source-buffer
	      source-window)))
	;; restore compilation buffer point
	(set-buffer compilation-last-buffer)
	(goto-char comp-buf-pt)

	(unless success
	  ;; none of the hooks handled the error
	  (error "error not recognized"))
	))))

(provide 'ada-fix-error)
;; end of file
