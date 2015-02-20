;;; hydra-examples.el --- Some applications for Hydra

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These are the sample Hydras.
;;
;; If you want to use them plainly, set `hydra-examples-verbatim' to t
;; before requiring this file. But it's probably better to only look
;; at them and use them as templates for building your own.

;;; Code:

(require 'hydra)

;;* Examples
;;** Example 1: text scale
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

;; This example generates three commands:
;;
;;     `hydra-zoom/text-scale-increase'
;;     `hydra-zoom/text-scale-decrease'
;;     `hydra-zoom/body'
;;
;; In addition, two of them are bound like this:
;;
;;     (global-set-key (kbd "<f2> g") 'hydra-zoom/text-scale-increase)
;;     (global-set-key (kbd "<f2> l") 'hydra-zoom/text-scale-decrease)
;;
;; Note that you can substitute `global-map' with e.g. `emacs-lisp-mode-map' if you need.
;; The functions generated will be the same, except the binding code will change to:
;;
;;     (define-key emacs-lisp-mode-map [f2 103]
;;       (function hydra-zoom/text-scale-increase))
;;     (define-key emacs-lisp-mode-map [f2 108]
;;       (function hydra-zoom/text-scale-decrease))

;;** Example 2: move window splitter
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-splitter (global-map "C-M-s")
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)))

;;** Example 3: jump to error
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

;; This example introduces only one new thing: since the command
;; passed to the "q" head is nil, it will quit the Hydra without doing
;; anything. Heads that quit the Hydra instead of continuing are
;; referred to as having blue :color. All the other heads have red
;; :color, unless other is specified.

;;** Example 4: toggle rarely used modes
(when (bound-and-true-p hydra-examples-verbatim)
  (global-set-key
   (kbd "C-c C-v")
   (defhydra hydra-toggle (:color blue)
     "toggle"
     ("a" abbrev-mode "abbrev")
     ("d" toggle-debug-on-error "debug")
     ("f" auto-fill-mode "fill")
     ("t" toggle-truncate-lines "truncate")
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel"))))

;; Note that in this case, `defhydra' returns the `hydra-toggle/body'
;; symbol, which is then passed to `global-set-key'.
;;
;; Another new thing is that both the keymap and the body prefix are
;; skipped.  This means that `defhydra' will bind nothing - that's why
;; `global-set-key' is necessary.
;;
;; One more new thing is that you can assign a :color to the body. All
;; heads will inherit this color. The code above is very much equivalent to:
;;
;;     (global-set-key (kbd "C-c C-v a") 'abbrev-mode)
;;     (global-set-key (kbd "C-c C-v d") 'toggle-debug-on-error)
;;
;; The differences are:
;;
;; * You get a hint immediately after "C-c C-v"
;; * You can cancel and call a command immediately, e.g. "C-c C-v C-n"
;;   is equivalent to "C-n" with Hydra approach, while it will error
;;   that "C-c C-v C-n" isn't bound with the usual approach.

;;** Example 5: mini-vi
(defun hydra-vi/pre ()
  (set-cursor-color "#e52b50"))

(defun hydra-vi/post ()
  (set-cursor-color "#ffffff"))

(when (bound-and-true-p hydra-examples-verbatim)
  (global-set-key
   (kbd "C-z")
   (defhydra hydra-vi (:pre hydra-vi/pre :post hydra-vi/post :color amaranth)
     "vi"
     ("l" forward-char)
     ("h" backward-char)
     ("j" next-line)
     ("k" previous-line)
     ("m" set-mark-command "mark")
     ("a" move-beginning-of-line "beg")
     ("e" move-end-of-line "end")
     ("d" delete-region "del" :color blue)
     ("y" kill-ring-save "yank" :color blue)
     ("q" nil "quit"))))

;; This example introduces :color amaranth. It's similar to red,
;; except while you can quit red with any binding which isn't a Hydra
;; head, you can quit amaranth only with a blue head. So you can quit
;; this mode only with "d", "y", "q" or "C-g".
;;
;; Another novelty are the :pre and :post handlers. :pre will be
;; called before each command, while :post will be called when the
;; Hydra quits. In this case, they're used to override the cursor
;; color while Hydra is active.

;;** Example 6: selective global bind
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-next-error (global-map "C-x")
    "next-error"
    ("`" next-error "next")
    ("j" next-error "next" :bind nil)
    ("k" previous-error "previous" :bind nil)))

;; This example will bind "C-x `" in `global-map', but it will not
;; bind "C-x j" and "C-x k".
;; You can still "C-x `jjk" though.
;;** Example 7: toggle with Ruby-style docstring
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-toggle (:color pink)
    "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_g_ golden-ratio-mode: %`golden-ratio-mode
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode

"
    ("a" abbrev-mode nil)
    ("d" toggle-debug-on-error nil)
    ("f" auto-fill-mode nil)
    ("g" golden-ratio-mode nil)
    ("t" toggle-truncate-lines nil)
    ("w" whitespace-mode nil)
    ("q" nil "quit"))
  (global-set-key (kbd "C-c C-v") 'hydra-toggle/body))

;; Here, using e.g. "_a_" translates to "a" with proper face.
;; More interestingly:
;;
;;     "foobar %`abbrev-mode" means roughly (format "foobar %S" abbrev-mode)
;;
;; This means that you actually see the state of the mode that you're changing.
;;** Example 8: the whole menu for `Buffer-menu-mode'
(defhydra hydra-buffer-menu (:color pink)
  "
  Mark               Unmark             Actions            Search
-------------------------------------------------------------------------                        (__)
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch                         (oo)
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch                      /------\\/
_d_: delete                           _g_: refresh       _O_: multi-occur                 / |    ||
_D_: delete up                        _T_: files only: % -28`Buffer-menu-files-only      *  /\\---/\\
_~_: modified                                                                               ~~   ~~
"
  ("m" Buffer-menu-mark nil)
  ("u" Buffer-menu-unmark nil)
  ("U" Buffer-menu-backup-unmark nil)
  ("d" Buffer-menu-delete nil)
  ("D" Buffer-menu-delete-backwards nil)
  ("s" Buffer-menu-save nil)
  ("~" Buffer-menu-not-modified nil)
  ("x" Buffer-menu-execute nil)
  ("b" Buffer-menu-bury nil)
  ("g" revert-buffer nil)
  ("T" Buffer-menu-toggle-files-only nil)
  ("O" Buffer-menu-multi-occur nil :color blue)
  ("I" Buffer-menu-isearch-buffers nil :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp nil :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))
;; Recommended binding:
;; (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

;;* Windmove helpers
(require 'windmove)

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(provide 'hydra-examples)
;;; hydra-examples.el ends here
