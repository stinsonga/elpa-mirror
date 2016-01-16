;;; gobject-minor-mode.el --- minor mode for editing GObject-style C source code -*- lexical-binding: t; -*-
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

(autoload 'gobject-align-at-point "gobject-align")
(autoload 'gobject-align-region "gobject-align")
(autoload 'gobject-align-set-column "gobject-align")
(autoload 'gobject-align-guess-columns "gobject-align")
(autoload 'gobject-snippet-insert-package_class "gobject-snippet")
(autoload 'gobject-snippet-insert-PACKAGE_CLASS "gobject-snippet")
(autoload 'gobject-snippet-insert-PackageClass "gobject-snippet")
(autoload 'gobject-snippet-insert-interface-declation "gobject-snippet")
(autoload 'gobject-snippet-insert-class-declation "gobject-snippet")
(autoload 'gobject-snippet-insert-set_property "gobject-snippet")
(autoload 'gobject-snippet-insert-get_property "gobject-snippet")
(autoload 'gobject-snippet-insert-dispose "gobject-snippet")
(autoload 'gobject-snippet-insert-finalize "gobject-snippet")
(autoload 'gobject-snippet-insert-notify "gobject-snippet")
(autoload 'gobject-snippet-insert-constructed "gobject-snippet")
(autoload 'gobject-snippet-insert "gobject-snippet")

(defvar gobject-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ga" 'gobject-align-at-point)
    (define-key keymap "\C-c\C-gr" 'gobject-align-region)
    (define-key keymap "\C-c\C-gf" 'gobject-align-set-column)
    (define-key keymap "\C-c\C-gg" 'gobject-align-guess-columns)
    (define-key keymap "\C-c\C-gc" 'gobject-snippet-insert-package_class)
    (define-key keymap "\C-c\C-gC" 'gobject-snippet-insert-PACKAGE_CLASS)
    (define-key keymap "\C-c\C-g\C-c" 'gobject-snippet-insert-PackageClass)
    (define-key keymap "\C-c\C-gs" 'gobject-snippet-insert)
    keymap))

;;;###autoload
(define-minor-mode gobject-minor-mode
  "A minor-mode for editing GObject-based C source code."
  nil " GObject" gobject-minor-mode-map)

(provide 'gobject-c-mode)

;;; gobject-c-mode.el ends here
