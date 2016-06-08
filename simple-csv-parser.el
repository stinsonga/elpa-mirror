;;; simple-csv-parser.el --- Simple CSV parser using parsec.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

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

;; Ref: http://book.realworldhaskell.org/read/using-parsec.html

;;; Code:

(defun s-csv-file ()
  (parsec-many (s-csv-line)))

(defun s-csv-line ()
  (prog1 (s-csv-cells)
    (s-csv-eol)))

(defun s-csv-eol ()
  (parsec-or (parsec-str "\n")
             (parsec-eob)))

(defun s-csv-cells ()
  (cons (s-csv-cell-content) (s-csv-remaining-cells)))

(defun s-csv-cell-content ()
  (parsec-many-as-string (parsec-re "[^,\n]")))

(defun s-csv-remaining-cells ()
  (parsec-or (parsec-and (parsec-ch ?,) (s-csv-cells)) nil))

(defun s-parse-csv (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (s-csv-file)))

(s-parse-csv "a1s,b,d,e,f")

(provide 'simple-csv-parser)
;;; simple-csv-parser.el ends here
