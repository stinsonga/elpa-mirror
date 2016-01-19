;;; gnome-c-style.el --- minor mode for editing GNOME-style C source code -*- lexical-binding: t; -*-
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

(require 'gnome-c-align)
(require 'gnome-c-snippet)

(defgroup gnome-c-style nil
  "GNOME-style C source code editing"
  :prefix "gnome-c-"
  :group 'c)

(defvar gnome-c-style-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ga" 'gnome-c-align-at-point)
    (define-key keymap "\C-c\C-gr" 'gnome-c-align-region)
    (define-key keymap "\C-c\C-gf" 'gnome-c-align-set-column)
    (define-key keymap "\C-c\C-gg" 'gnome-c-align-guess-columns)
    (define-key keymap "\C-c\C-g\C-g" 'gnome-c-align-compute-optimal-columns)
    (define-key keymap "\C-c\C-gc" 'gnome-c-snippet-insert-package_class)
    (define-key keymap "\C-c\C-gC" 'gnome-c-snippet-insert-PACKAGE_CLASS)
    (define-key keymap "\C-c\C-g\C-c" 'gnome-c-snippet-insert-PackageClass)
    (define-key keymap "\C-c\C-gs" 'gnome-c-snippet-insert)
    keymap))

;;;###autoload
(define-minor-mode gnome-c-style-mode
  "A minor-mode for editing GNOME-style C source code."
  nil " GNOME" gnome-c-style-mode-map)

(provide 'gnome-c-style)

;;; gnome-c-style.el ends here
