;;; Insert texinfo index entries into arm*.texinfo -*- lexical-binding:t -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>

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

(defun insert-index-entries ()
  "Read the index @chapter, insert @cindex entries in sections."
  ;; The texinfo file is produced by the arm_form executable; see
  ;; arm_texi.adb for some info.
  (goto-char (point-min))

  ;; First add the menu entry
  (search-forward "@menu")
  (search-forward "@end menu")
  (goto-char (line-beginning-position))
  (insert "* Concept Index :: Concept Index\n")

  ;; now the command to build the index and print it
  (goto-char (point-max))
  (goto-char (line-beginning-position 0))
  (insert "@node Concept Index\n")
  (insert "@unnumbered Concept Index\n")
  (insert "@printindex cp\n")

  ;; Now insert all the @cindex items
  (search-backward "@chapter Index")

  (let (next-search-pos)
    (while (search-forward-regexp "@ref{ \\([0-9]+\\), \\([0-9.(/)]+\\)}" nil t)
      (setq next-search-pos (copy-marker (point))) ;; we are inserting text before this point!

      ;; There can be more than one @ref on a line; they are all for the
      ;; same entry; texinfo can handle that.
      (let ((ref (match-string-no-properties 1))
	    text)
	(goto-char (line-beginning-position))
	(when (looking-at "\\(?:@w{ }\\)*\\(.*?\\)@w{ }")
	  (setq text (match-string-no-properties 1))
	  (goto-char (point-min))
	  (search-forward (concat "@anchor{ " ref "}"))
	  (insert (concat "\n@cindex " text "\n")))
	(goto-char next-search-pos)))))

(defun do-index (filename)
  (find-file filename)
  (insert-index-entries)
  (save-buffer))

(provide 'arm-texi-index)
;;; ada-mode.el ends here
