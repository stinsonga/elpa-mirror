;;; full-csv-parser.el --- Sample csv parser using parsec.el  -*- lexical-binding: t; -*-

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

(defun csv-file ()
  (parsec-ensure
      (parsec-return (parsec-endby (csv-line) (csv-eol))
        (parsec-eob))))

(defun csv-line ()
  (parsec-sepby (csv-cell) (parsec-ch ?,)))

(defun csv-cell ()
  (parsec-or (csv-quoted-cell) (parsec-many-as-string (parsec-re "[^,\n\r]"))))

(defun csv-quoted-cell ()
  (parsec-and (parsec-ch ?\")
          (parsec-return (parsec-many-as-string (csv-quoted-char))
            (parsec-ensure (parsec-ch ?\")))))

(defun csv-quoted-char ()
  (parsec-or (parsec-re "[^\"]")
             (parsec-and (parsec-str "\"\"")
                         "\"")))

(defun csv-eol ()
  (parsec-or (parsec-str "\n\r")
             (parsec-str "\r\n")
             (parsec-str "\n")
             (parsec-str "\r")
             (parsec-eob)))

(defun parse-csv (input)
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (csv-file)))

(parse-csv "\"a,1,s\"s,b,\r\nd,e,f")
(parse-csv "\"e\"\",f")
(parse-csv "\"a,1,\r\n")
(parse-csv "\"a,1,\"\",b,\r\nd,,f")

(provide 'full-csv-parser)
;;; full-csv-parser.el ends here
