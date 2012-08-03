;; -*- lexical-binding: t -*-
;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Christopher Schmidt <christopher@ch.ristopher.com>
;; Maintainer: Christopher Schmidt <christopher@ch.ristopher.com>
;; Created: 2012-03-08
;; Compatibility: GNU Emacs: 24.x

;; This file is part of ampc.

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
;; I use this file to generate the formatted EmacsWiki description of ampc from
;; the `Commentary' section in ampc.el.

;;; Code:
(let ((data))
  (split-window-below)
  (other-window 1)

  (switch-to-buffer (get-buffer-create " *ampc EmacsWiki*"))

  (delete-region (point-min) (point-max))

  (with-temp-buffer
    (insert-file-contents
     (find-lisp-object-file-name 'ampc (symbol-function 'ampc)))
    (delete-region (point-min)
                   (progn (search-forward-regexp "^;;; \\*\\* installation")
                          (move-beginning-of-line nil)))
    (delete-region (progn (search-forward-regexp "^;;; Code:\n")
                          (previous-line 2)
                          (move-beginning-of-line nil)
                          (point))
                   (point-max))
    (goto-char (point-min))
    (replace-regexp "^;; ?" "")
    (goto-char (point-min))
    (replace-regexp "^; \\*\\* \\(.*\\)" "== \\1 ==\n")
    (goto-char (point-min))
    (replace-regexp "^; \\*\\*\\* \\(.*\\)" "=== \\1 ===\n")
    (goto-char (point-min))
    (replace-regexp "^\n\\(?1:\\((\\|\\.\\)\\(.*\n\\)+?\\)\n"
                    "\n<pre>\n\\1</pre>\n\n")
    (goto-char (point-min))
    (replace-regexp "ELPA" "[[ELPA]]")
    (goto-char (point-min))
    (replace-regexp "\\(this\\( \\|\n\\)file\\)" "\\1 (Lisp:ampc.el)")
    (setf data (buffer-string)))
  (insert data))

;; Local Variables:
;; fill-column: 80
;; indent-tabs-mode: nil
;; End:
