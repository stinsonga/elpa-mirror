;;; greenbar.el --- Mark comint output with "greenbar" background -*- lexical-binding: t -*-

;; Copyright (C) 2013-2020  Free Software Foundation, Inc.

;; Author: Michael R. Mauger <michael@mauger.com>
;; Version: 1.0
;; Package-Type: simple
;; Keywords: faces, terminals

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

;; For some of us old neck beards who learned to write software on
;; pucch cards and print out our code and output on wide line
;; printers.  When reading long rows of text across a 14 7/8" page it
;; was helpful to have alternating bands of subtle background coloring
;; to guide your eyes across the line.  This is also referred to as
;; `zebra striping` and is enabled in on PostScript output in
;; `ps-print.el' by enabling the `ps-zebra-stripes' setting.

;; To enable `greenbar-mode' in your `comint-mode' buffers, add the
;; following to your Emacs configuration:

;;     (add-hook 'comint-mode-hook #'greenbar-mode)

;; If you want to enable `greenbar-mode' only in a single mode derived
;; from `comint-mode', then you need to add `greenbar-mode' only to
;; the desired derive mode hook.  Adding `greenbar-mode' to
;; `comint-mode-hook' enables it for all comint derived modes.

;; The variable `greenbar-color-theme' is a list of predefined bar
;; background colors.  Each element of the list is a list: the first
;; member of which is a symbol that is the name of the theme; the rest
;; of the list are color names which are used as background colors for
;; successive bands of lines.

;; The variable `greenbar-color-list' controls which set of color bars
;; are to be applied.  The value is either a name from color theme
;; defined in `greenbar-color-themes' or it is a list of color names.

;;; Code:

(require 'comint)

(defgroup greenbar nil
  "Stripe comint output like \"green bar\", or \"zebra stripe\" paper."
  :group 'comint)

(defvar-local greenbar-current-bar 0
  "Index into `greenbar-background-colors' that is active.")

(defvar-local greenbar-current-line 0
  "The line into the bar that is active.")

(defcustom greenbar-lines-per-bar 3
  "How many lines of output should be colored together."
  :type 'integer)

(defvar greenbar-color-themes
  (list
   (cons 'greenbar
         (if (eq (frame-parameter nil 'background-mode) 'dark)
             '("#344034" "#343434")
           '("#e4f0e4" "#f0f0f0")))
   (cons 'graybar
         (list
          (if (eq (frame-parameter nil 'background-mode) 'dark)
              "gray30" "gray70")
          (face-background 'default)))
   (cons 'rainbow
         (let ((x (if (eq (frame-parameter nil 'background-mode) 'dark) "40" "f0"))
               (o (if (eq (frame-parameter nil 'background-mode) 'dark) "34" "e4")))

           (mapcar (lambda (c) (apply #'concat "#" c))
                   `((,x ,x ,o) (,x ,o ,o) (,x ,o ,x) (,o ,o ,x) (,o ,x ,x) (,o ,x ,o))))))
  "Greenbar themes.

A list of greenbar themes, each of which is a list starting with
a symbol that names the theme followed by the list bar colors.")

(defcustom greenbar-background-colors 'greenbar
  "List of background colors to be applied to output stripes."
  :type `(choice ,@(mapcar (lambda (c)
                             (list 'const (car c)))
                           greenbar-color-themes)
                 (repeat (color :tag "Background list"))))

(defun greenbar-color-list ()
  "Get the list of greenbar background colors."
    (or (cdr (assoc greenbar-background-colors
                    greenbar-color-themes))
        greenbar-background-colors))

(defun greenbar-next-bar ()
  "Reset the local configuration if we are at the end of a bar.

If `greenbar-lines' is zero, reset it to
`greenbar-lines-per-bar', and move `greenbar-current-bar' to the
next one."

  (when (zerop greenbar-current-line)
    (setq greenbar-current-bar (mod (1+ greenbar-current-bar) (length (greenbar-color-list)))
          greenbar-current-line (default-value 'greenbar-lines-per-bar))))

(defun greenbar-output-filter (string)
  "Stripe comint output in STRING with background colors.

Every `greenbar-lines-per-bar' lines are colored with a rotating
set of background colors found in
`greenbar-background-colors'."

  (let ((bg-list (greenbar-color-list))
        (start comint-last-output-start)
        (end (process-mark (get-buffer-process (current-buffer)))))

    ;;(message "greenbar: %S %S %S" start end (replace-regexp-in-string "\n" "\\\\n" (buffer-substring start end)))
    (when (and bg-list (listp bg-list) (not (= start end)))
      (greenbar-next-bar) ; make sure greenbar state is valid
      (save-excursion
        (save-restriction
          ;; Don't highlight partial last line
          (goto-char end)
          (forward-line 0)
          (setq end (point))

          ;; Highlight the beginning of the start line
          (goto-char start)
          (forward-line 0)
          (setq start (point))

          ;; Limit what we can highlight
          (narrow-to-region start end)

          ;; Mark every set of lines alternating among bar colors
          (while (< start end)
            (goto-char start)
            (setq greenbar-current-line (forward-line greenbar-current-line))

            ;; Mark the bar
            (let ((bar-face (nth greenbar-current-bar bg-list)))
              (font-lock-append-text-property
               start (point)
               'font-lock-face (list :background bar-face
                                     :extend t)))

            ;; Get ready for the next bar
            (setq start (point))

            ;; When the full bar is complete, set up for next bar
            (greenbar-next-bar))))))
  string)

;;;###autoload
(define-minor-mode greenbar-mode
  "Enable \"green bar striping\" of comint output"
  nil nil nil
  (if greenbar-mode
      (add-hook 'comint-output-filter-functions
                #'greenbar-output-filter t t)
    (remove-hook 'comint-output-filter-functions
                 #'greenbar-output-filter t)))

(provide 'greenbar)

;;; greenbar.el ends here
