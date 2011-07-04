;;; sisu-mode.el --- a major-mode for highlighting a hierarchy structured text.

;; Copyright (C) 2011  Free Software Foundation, Inc.

;; Author: Ambrose Kofi Laing (& Ralph Amissah)
;; Keywords: text, processes, tools
;; Version: 3.0.3 2011-03-11 (2008-12-14)
;; License: GPLv3
;; Home URL: SiSU:   http://www.jus.uio.no/sisu
;; originally looked at (based on) doc-mode, with kind permission of the author
;;   Author: SUN, Tong <suntong001@users.sf.net>, (c)2001-6, all right reserved
;;   Version: $Date: 2006/01/19 03:13:41 $ $Revision: 1.14 $
;;   Home URL: http://xpt.sourceforge.net/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Viva Software Libre!
;; Support the free software movement!
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:

;;
;; To enter sisu-mode automatically, add (autoload 'sisu-mode "sisu-mode")
;; to your .emacs file and change the first line of your sisu file to:
;; # -*- Sisu -*-
;; if it doesn't have a .sisu extension.
;;
;; To handle .sisu files, e.g., 'filename.sisu', add something like
;; (add-to-list 'auto-mode-alist '("\\.sisu$" . sisu-mode))
;; to your .emacs file

;;
;; The documentation for the "Structure Of The Hierarchy Text" can be found in
;; the sisustring for the sisu-mode function.
;;

;;; Code:

;;{{{ Variables:

(defgroup sisu-faces nil
  "AsciiSisu highlighting"
  :group 'sisus)

;; == Colors
; color n is more prominent than color n+1

(defface sisu-title-1-face
  `((((class color)
      (background dark))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiSisu titles at level 1."
  :group 'sisu-faces)

(defface sisu-title-2-face
  `((((class color)
      (background dark))
     (:foreground "yellow4" :bold t :height 1.1 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "yellow4" :bold t :height 1.1 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiSisu titles at level 2."
  :group 'sisu-faces)

(defface sisu-title-3-face
  `((((class color)
      (background dark))
     (:foreground "sienna3" :bold t))
    (((class color)
      (background light))
     (:foreground "sienna3" :bold t))
    (t (:weight bold)))
  "Face for AsciiSisu titles at level 3."
  :group 'sisu-faces)

(defface sisu-title-4-face
  `((((class color)
      (background dark))
     (:foreground "burlywood3"))
    (((class color)
      (background light))
     (:foreground "burlywood3"))
    (t ()))
  "Face for AsciiSisu titles at level 4."
  :group 'sisu-faces)

(defface info-node
  '((((class color) (background light)) (:foreground "brown" :bold t :italic t))
    (((class color) (background dark)) (:foreground "white" :bold t :italic t))
    (t (:bold t :italic t)))
  "Face for Info node names."
  :group 'sisu-faces)

(defvar sisu-title-1 'sisu-title-1-face)
(defvar sisu-title-2 'sisu-title-2-face)
(defvar sisu-title-3 'sisu-title-3-face)
(defvar sisu-title-4 'sisu-title-4-face)

(defvar general-font-lock-red1 font-lock-warning-face)
(defvar general-font-lock-red2 font-lock-comment-face)
(defvar general-font-lock-red3 font-lock-string-face)

(defvar general-font-lock-green1 font-lock-type-face)
(defvar general-font-lock-green2 font-lock-constant-face)

(defvar general-font-lock-blue1 font-lock-keyword-face)
(defvar general-font-lock-blue2 font-lock-function-name-face)
(defvar general-font-lock-blue3 font-lock-builtin-face)

(defvar general-font-lock-yellow1 font-lock-variable-name-face)
(defvar general-font-lock-yellow2 font-lock-comment-face)

;; == sisu-mode settings

(defvar sisu-mode-hook nil
  "Normal hook run when entering Sisu Text mode.")

(defvar sisu-mode-abbrev-table nil
  "Abbrev table in use in Sisu-mode buffers.")
(define-abbrev-table 'sisu-mode-abbrev-table ())

(defconst sisu-font-lock-keywords
  (eval-when-compile
    (list

     ;;grouped text
     (cons "^group\{\\|^\}group"       'general-font-lock-red2)
     (cons "^block\{\\|^\}block"       'general-font-lock-red2)
     (cons "^code\{\\|^\}code"         'general-font-lock-red2)
     (cons "^poem\{\\|^\}poem"         'general-font-lock-red2)
     (cons "^alt\{\\|^\}alt"           'general-font-lock-red2)
     (cons "^table\{.+\\|^\}table"     'general-font-lock-red2)
     (cons "^\{table[^}]+\}"           'general-font-lock-red2)

     ;; footnote/endnote
       ;(cons "\~\{.+?\}\~"  'general-font-lock-green1)
     (cons "\~\{\\*\\*\\|\~\{\\*\\|\~\{\\|\}\~"   'general-font-lock-red2)
     (cons "\~\\[\\+\\|\~\\[\\*\\|\~\\[\\|\\]\~"  'general-font-lock-red2)

     (cons "\~\\^ \\|^\\^\~ " 'general-font-lock-red2)

     (list (concat
      "\\(\*\~\\)"
      "\\([^ \r\t\n]+\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-blue2 t))

     ;; emphasis (can be program configured to be bold italics or underscore)
     (list (concat
      "\\([*]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[*]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
     '(3 general-font-lock-red1 t))

     ;; bold
     (list (concat
      "\\([!]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[!]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
     '(3 general-font-lock-red1 t))
     (cons "\\*[^ ]+\\*"               'general-font-lock-red1)
     (cons "^!_ .+"                    'general-font-lock-red1)

     ;;; italics
     (list (concat
      "\\([/]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[/]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-blue1 t)
           '(3 general-font-lock-red1 t))

     ;; underscore
     (list (concat
      "\\([_]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[_]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; monospace
     (list (concat
      "\\([#]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[#]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; citation
     (list (concat
      "\\([\"]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[\"]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; inserted text
     (list (concat
      "\\([\+]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[\+]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; strike through
     (list (concat
      "\\(\\-\{\\)"
      "\\([^\}]+\\)"
      "\\(\}\\-\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; superscript
     (list (concat
      "\\(\\^\{\\)"
      "\\([^\}]+\\)"
      "\\(\}\\^\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;; subscript
     (list (concat
      "\\([,]\{\\)"
      "\\([^\}]+\\)"
      "\\(\}[,]\\)")
     '(1 general-font-lock-red1 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-red1 t))

     ;;numbered list
     (cons "^# \\|^_# "                'general-font-lock-red1)

     ;;bullet text
     (cons "^_\\*[1-9] \\|^_\\* "      'general-font-lock-red1)

     ;;indented text
     (cons "^_[1-9] "                  'general-font-lock-red1)

     ;;url
     (cons "\\(^\\|[ ]\\)http:[/][/][^ \t\n\r<]+" 'general-font-lock-blue2)

;; \\|\$

     ;; Comment Lines
     (cons "^% .*"                     'general-font-lock-blue1)
     ;; line break
     (cons "<br>"                      'general-font-lock-red1)

     ;; Section titles
     (list "^\\(\\([1-8]\\|:?[A-C]\\)\\~\\)\\(.*\\)"
     '(1 sisu-title-1 t)
     '(3 sisu-title-2 t))

     ;; hyper-links
     (list (concat
      "\\(\{~^\\|\{\\)"
      "\\([^\}\{]+\\)"
      "\\(\}http:[/][/][^ \r\n\t<]+\\)")
     '(1 general-font-lock-blue2 t)
     '(2 general-font-lock-red1 t)
     '(3 general-font-lock-blue2 t))

     ;; book index
     (cons "^\=\{.+\}"                 'general-font-lock-green1)

     ;; numbers
     (cons "\\<[.0-9]+\\>"             'general-font-lock-green2)

     ;; bullets sisu_normal (nearly copied regexp)
     (cons "^_\\([1-9*]\\|[1-9]\\*\\) " 'general-font-lock-blue2)

     ;; image links
     (list (concat
      "\\(\{\\)"
      "\\([^\}\{]+\\)"
      "\\(\}image\\)")
     '(1 general-font-lock-blue2 t)
     '(2 general-font-lock-red1 t)
           '(3 general-font-lock-blue2 t))

     ;; insert file links
     (list (concat
      "\\(<< \\)"
      "\\([^ \r\t\n]+\\.ss\\)"
      "\\(i\\|t\\)")
     '(1 general-font-lock-blue2 t)
     '(2 general-font-lock-blue2 t)
           '(3 general-font-lock-blue2 t))

     ;; raw keywords
     (list (concat
      "^\\(\\@\\("
      "title\\|"
      "creator\\|"
      "date\\|"
      "publisher\\|"
      "rights\\|"
      "classify\\|"
      "original\\|"
      "notes\\|"
      "links\\|"
      "make\\|"
      "\\):\\)\\(.*\\)")
     '(1 sisu-title-2 keep)
     '(3 sisu-title-3 keep))

     ))
 "Default expressions to highlight in AsciiSisu mode.")

;;}}}

;;{{{ Sisu & Autoload:

;;###autoload
(define-derived-mode sisu-mode text-mode "SiSU"
  "Major mode for editing SiSU files."
  (interactive)
  (modify-syntax-entry ?\'  ".")
  ;(flyspell-mode nil)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|>" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
  '(sisu-font-lock-keywords
    nil        ; KEYWORDS-ONLY: no
    nil        ; CASE-FOLD: no
    ((?_ . "w"))      ; SYNTAX-ALIST
    ))
  (run-hooks 'sisu-mode-hook))

(provide 'sisu-mode)

;;}}}

;;; sisu-mode.el ends here
