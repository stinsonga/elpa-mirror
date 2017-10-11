;;; hook-helpers-use-package.el --- Support for use-package -*- lexical-binding: t; -*-

;; Copyright (c) 2017 Ian Dunn

;; Author: Ian Dunn <scholar42@gmail.com>
;; Version: 1.0

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds support for use-package using the :hkhlp keyword.

;; Sets the current package's mode as a hook run by other hooks.

;; Examples:

;; (use-package outline
;;   :hkhlp
;;   (:cmd outline-minor-mode
;;    prog-mode
;;    text-mode))

;; Adds a hook helper that runs outline-minor-mode, and adds it to
;; prog-mode-hook and text-mode-hook.

;; (use-package flyspell
;;   :hkhlp
;;   (text-mode-hook
;;    erc-mode-hook
;;    lui-mode-hook
;;    jabber-chat-mode-hook
;;    :cmd flyspell-prog-mode
;;    prog-mode))

;; Creates two hook helpers: The first attaches flyspell-mode to text-mode-hook,
;; erc-mode-hook, lui-mode-hook, and jabber-chat-mode-hook.  The second attaches
;; flyspell-prog-mode to prog-mode-hook.

;;; Code:

(require 'map)
(require 'use-package nil t)

(defun hkhlp-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise
convert it to a string and return that."
  (if (stringp string-or-symbol) string-or-symbol
    (symbol-name string-or-symbol)))

(defun hkhlp-as-hook (string-or-symbol)
  "If STRING-OR-SYMBOL ends in `-hook' (or its name does), return
it as a symbol.  Otherwise, return it as a symbol with `-hook'
appended."
  (let ((string (hkhlp-as-string string-or-symbol)))
    (intern (if (string-match-p "-hook\\'" string) string
              (concat string "-hook")))))

(defun hkhlp-normalize-form (args)
  (let (form
        current-command
        found-command)
    (dolist (arg args form)
      (pcase arg
        (':cmd
         (setq found-command t))
        (_
         (if found-command
             (setq found-command nil
                   current-command arg)
           (push (hkhlp-as-hook arg)
                 (alist-get current-command form))))))))

(defun hkhlp-init-form (args)
  (map-apply
   (lambda (func hooks)
     `(create-hook-helper ,func ()
        :hooks (,@hooks)
        (,func)))
   (hkhlp-normalize-form args)))

(cl-pushnew :hkhlp use-package-keywords)

(defun use-package-normalize/:hkhlp (name _keyword args)
  (cond
   ((symbolp args) (list :cmd (use-package-as-mode name) args))
   ((listp args)
    (if (eq (car args) :cmd)
        args
      (apply
       'append
       (list :cmd (use-package-as-mode name))
       args)))))

(defun use-package-handler/:hkhlp (name _keyword arg rest state)
  (let ((forms (hkhlp-init-form arg))
        (cmds (mapcar #'car (hkhlp-normalize-form arg))))
    ;; Defer loading of this package until activated by the other hooks
    (setq rest (use-package-plist-maybe-put rest :defer t))
    ;; Add forms to :init
    (setq rest (use-package-plist-append rest :init forms))
    (use-package-process-keywords name
      (use-package-sort-keywords rest)
      ;; Add the commands to state for autoloading
      (use-package-plist-append state :commands cmds))))

(provide 'hook-helpers-use-package)

;;; hook-helpers-use-package.el ends here
