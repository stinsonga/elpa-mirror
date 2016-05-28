;;; hook-helpers.el --- Functions and macros to help with handling hooks

;; Copyright (C) 2016 Ian Dunn

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: development, hooks
;; URL: https://savannah.nongnu.org/projects/hook-helpers-el/
;; Version: 1.0
;; Created: 06 May 2016
;; Modified: 21 May 2016

;; This program is free software: you can redistribute it and/or modify
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

;; Often times, I see people define a function to be used once in a hook.  If
;; they don’t do this, then it will be an anonymous function.  If the anonymous
;; function is modified, then the function can’t be removed.  With a function
;; outside of the ‘add-hook’ call, it looks messy.

;; The ‘define-hook-helper’ macro is a solution to this.  Think of it as an
;; anaphoric ‘add-hook’, but one that can be called many times without risking
;; redundant hook functions.  It gives a cleaner look and feel to Emacs
;; configuration files, and could even be used in actual libraries.

;;; Code:

(defconst hook-helper--helper-prefix "hook-helper")

;;;###autoload
(defmacro define-hook-helper (hook args &optional docstring &rest body)
  "Define a hook helper for the variable HOOK-hook with ARGS as the argument list.

This helper consists of all the code in BODY.  HOOK should not be
quoted.  The keywords are:

:name    Specifies a name to use for the generated function.  As part
         of this macro, a function called hook-helper--HOOK will be
         created.  If NAME is given, then the function becomes
         ‘hook-helper--HOOK/NAME’.

:append  If non-nil, append the hook helper to the hook variable.

:suffix  Allows a user to specify that the hook variable doesn't
         end with ‘-hook’, but instead with another suffix, such as
         ‘-function’.  SUFFIX should be a string, and defaults to ‘hook’
         if not specified.  Note that SUFFIX is not assumed to start with
         a hyphen."
  (declare (indent defun) (doc-string 3))
  ;; From `define-derived-mode'
  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))
  ;; Process the key words
  (let ((name nil)
        (append nil)
        (suffix "hook"))
    (while (keywordp (car body))
      (pcase (pop body)
	(`:name (setq name (pop body)))
	(`:append (setq append (pop body)))
	(`:suffix (setq suffix (pop body)))
	(_ (pop body))))
    (let ((func-sym (intern (format "%s--%s%s" hook-helper--helper-prefix (symbol-name hook) (if name (concat "/" (symbol-name name)) "")))))
      `(progn
         (defun ,func-sym ,args
           ,(format "Function to run for %s-%s" (symbol-name hook) suffix)
           ,@body)
         (add-hook (quote ,(intern (concat (symbol-name hook) "-" suffix)))
                   (function ,func-sym)
                   ,append)))))

(cl-defmacro remove-hook-helper (hook &key name (suffix "hook"))
  "Remove a hook helper from HOOK-hook.

NAME and SUFFIX are exactly as in ‘define-hook-helper’, and can
be used to find the exact helper to remove."
  (let ((func-sym (intern (format "%s--%s%s" hook-helper--helper-prefix (symbol-name hook) (if name (concat "/" (symbol-name name)) "")))))
    `(remove-hook (quote ,(intern (concat (symbol-name hook) "-" suffix))) (function ,func-sym))))

;;;###autoload
(defmacro define-hook-function (function args &optional docstring &rest body)
  "Define FUNCTION to be a function, then add it to hooks.

The hooks to add are specified by the :hooks keyword.  This is a
simple list of hooks, unquoted, and the new function is added to
each one."
  (declare (indent defun) (doc-string 3))
  ;; From `define-derived-mode'
  (when (and docstring (not (stringp docstring)))
    ;; Some trickiness, since what appears to be the docstring may really be
    ;; the first element of the body.
    (push docstring body)
    (setq docstring nil))
  ;; Process the key words
  (let ((hooks nil))
    (while (keywordp (car body))
      (pcase (pop body)
        ;; Hooks is a keyword to allow it to be specified, without requiring the
        ;; docstring.
        (`:hooks (setq hooks (pop body)))
	(_ (pop body))))
    `(progn
       (defun ,function ,args
         ,docstring
         ,@body)
       (dolist (h (quote ,hooks))
         (add-hook h (function ,function))))))

;; Add font lock for both macros.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\(define-hook-helper\\)\\_>[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))
   ("(\\(define-hook-function\\)\\_>[ \t]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    (1 font-lock-keyword-face)
    (2 font-lock-function-name-face nil t))))

(provide 'hook-helpers)

;;; hook-helpers.el ends here
