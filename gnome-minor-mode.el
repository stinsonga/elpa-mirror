;;; gnome-minor-mode.el --- minor mode for editing GNOME-style C source code -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Daiki Ueno <ueno@gnu.org>

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style
;; Version: 0.1
;; Maintainer: Daiki Ueno <ueno@gnu.org>

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

(autoload 'gnome-align-at-point "gnome-align")
(autoload 'gnome-align-region "gnome-align")
(autoload 'gnome-align-set-column "gnome-align")
(autoload 'gnome-align-guess-columns "gnome-align")
(autoload 'gnome-align-compute-optimal-columns "gnome-align")
(autoload 'gnome-snippet-insert-package_class "gnome-snippet")
(autoload 'gnome-snippet-insert-PACKAGE_CLASS "gnome-snippet")
(autoload 'gnome-snippet-insert-PackageClass "gnome-snippet")
(autoload 'gnome-snippet-insert-interface-declation "gnome-snippet")
(autoload 'gnome-snippet-insert-class-declation "gnome-snippet")
(autoload 'gnome-snippet-insert-set_property "gnome-snippet")
(autoload 'gnome-snippet-insert-get_property "gnome-snippet")
(autoload 'gnome-snippet-insert-dispose "gnome-snippet")
(autoload 'gnome-snippet-insert-finalize "gnome-snippet")
(autoload 'gnome-snippet-insert-notify "gnome-snippet")
(autoload 'gnome-snippet-insert-constructed "gnome-snippet")
(autoload 'gnome-snippet-insert "gnome-snippet")

(defvar gnome-minor-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ga" 'gnome-align-at-point)
    (define-key keymap "\C-c\C-gr" 'gnome-align-region)
    (define-key keymap "\C-c\C-gf" 'gnome-align-set-column)
    (define-key keymap "\C-c\C-gg" 'gnome-align-guess-columns)
    (define-key keymap "\C-c\C-g\C-g" 'gnome-align-compute-optimal-columns)
    (define-key keymap "\C-c\C-gc" 'gnome-snippet-insert-package_class)
    (define-key keymap "\C-c\C-gC" 'gnome-snippet-insert-PACKAGE_CLASS)
    (define-key keymap "\C-c\C-g\C-c" 'gnome-snippet-insert-PackageClass)
    (define-key keymap "\C-c\C-gs" 'gnome-snippet-insert)
    keymap))

;;;###autoload
(define-minor-mode gnome-minor-mode
  "A minor-mode for editing GNOME-style C source code."
  nil " GNOME" gnome-minor-mode-map)

(provide 'gnome-c-mode)

;;; gnome-c-mode.el ends here
