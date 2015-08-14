;;; minor mode to enable global prefix keys for other frame/window buffer placement -*- lexical-binding: t -*-
;;
;; Copyright (C) 2015  Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@member.fsf.org>
;; Maintainer: Stephen Leake <stephen_leake@member.fsf.org>
;; Keywords: frame
;; window
;; Version: 1.0.0
;; package-requires: ((emacs "25.0"))
;;
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
;;
;;; Usage:
;;
;; Enable the minor mode with:
;;
;; M-x other-frame-window-mode
;;
;; or, in your ~/.emacs:
;;
;; (other-frame-window-mode t)
;;
;; C-x 7 <command> causes a buffer displayed by <command> to appear in
;; another window in the same frame; a window is created if necessary.
;;
;; C-x 9 <command> causes a buffer displayed by <command> to appear in
;; another frame; a frame is created if necessary.

;;; Design:
;;
;; This uses C-x 7, 9 prefix because those keys are undefined in core
;; Emacs.  It could eventually switch to 4, 5, since those are
;; currently used for -other-window, -other-frame bindings.
;;
;; (info "(emacs) Pop Up Window") (info "(emacs) Creating Frames")
;;
;; This adds advice to switch-to-buffer; eventually Emacs could
;; reimplement switch-to-buffer to do the same.

;;; Code:

(defun ofw-add-to-overriding (func)
  "Add FUNC to 'display-buffer-overriding-action' action list."
  (let ((functions (car display-buffer-overriding-action))
	(attrs (cdr display-buffer-overriding-action)))
    (push func functions)
    (setq display-buffer-overriding-action (cons functions attrs))))

(defun ofw-delete-from-overriding ()
  "Delete 'ofw-display-buffer-other-window' and
'ofw-display-buffer-other-frame' from
'display-buffer-overriding-action' action list, if present."
    (let ((functions (car display-buffer-overriding-action))
	  (attrs (cdr display-buffer-overriding-action)))
      (setq functions (delq 'ofw-display-buffer-other-frame (delq 'ofw-display-buffer-other-window functions)))
      (setq display-buffer-overriding-action (cons functions attrs))))

(defun ofw-other-window ()
  "Set `display-buffer-overriding-action' to indicate other window."
  (interactive)
  (ofw-add-to-overriding 'ofw-display-buffer-other-window))

(defun ofw-other-frame ()
  "Set `display-buffer-overriding-action' to indicate other frame."
  (interactive)
  (ofw-add-to-overriding 'ofw-display-buffer-other-frame))

(defun ofw-display-buffer-other-window (buffer alist)
  "Show BUFFER in another window in the current frame,
creating new window if needed and allowed.
If successful, return window; else return nil.
Intended for 'display-buffer-overriding-action'."
  ;; Reset for next display-buffer call. FIXME: C-g, and next command
  ;; failure, should also reset this
  (setq display-buffer-overriding-action nil)

  ;; We can't use display-buffer-use-some-window here, because
  ;; that unconditionally allows another frame.
  (or (display-buffer-use-some-frame
       buffer
       (append (list (cons 'frame-predicate (lambda (frame) (eq frame (selected-frame))))
		     '(inhibit-same-window . t))
	       alist))
      (display-buffer-pop-up-window buffer alist)))

(defun ofw-display-buffer-other-frame (buffer alist)
  "Show BUFFER in another frame, creating a new frame
if needed.
If successful, return window; else return nil.
Intended for 'display-buffer-overriding-action'."
  ;; Reset for next display-buffer call.
  (setq display-buffer-overriding-action nil)

  (or (display-buffer-use-some-frame buffer alist)
      (display-buffer-pop-up-frame buffer alist)))

;; FIXME: use defadvice for Emacs 24.3
(defun ofw-switch-to-buffer-advice (_orig-fun buffer  &optional norecord _force-same-window)
  "Change switch-to-buffer to call pop-to-buffer.
This allows switch-to-buffer to respect 'ofw-other-window',
'ofw-other-frame'."
  (pop-to-buffer buffer (list 'display-buffer-same-window) norecord))

;; FIXME: use defadvice for Emacs 24.3
(defun ofw-temp-window-advice (orig-func buffer &optional action)
  "Delete any ofw actions from 'display-buffer-overriding-action',
call ORIG-FUNC, restore ofw actions to 'display-buffer-overriding-action'."
  (let ((temp display-buffer-overriding-action)
	result)
    (ofw-delete-from-overriding)
    (setq result (funcall orig-func buffer action))
    (setq display-buffer-overriding-action temp)
    result))

(defun ofw-move-to-other-window ()
  "Move current buffer to another window in same frame.
Point stays in moved buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-prev-buffer nil 'bury)
    (pop-to-buffer
     buffer
     (cons '(display-buffer-use-some-frame display-buffer-pop-up-window)
	   (list (cons 'frame-predicate (lambda (frame) (eq frame (selected-frame))))
		 '(inhibit-same-window . t)))
     )))

(defun ofw-move-to-other-frame ()
  "Move current buffer to a window in another frame.
Point stays in moved buffer."
  (interactive)
  (let ((buffer (current-buffer)))
    (switch-to-prev-buffer nil 'bury)
    (pop-to-buffer
     buffer
     (cons '(display-buffer-use-some-frame display-buffer-pop-up-frame)
	   '((reusable-frames . visible)))
     )))

(defvar ofw-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x7" 'ofw-other-window)
    (define-key map "\C-x9" 'ofw-other-frame)
    (define-key map "\C-xW" 'ofw-move-to-other-window)
    (define-key map "\C-xF" 'ofw-move-to-other-frame)
    map
  )  "Local keymap used for other-frame-window minor mode.")

(define-minor-mode other-frame-window-mode
  "Minor mode for other frame/window buffer placement.
Enable mode if ARG is positive."
  :init-value nil
  :lighter    " ofw"   ;; mode line
  :keymap     ofw-map
  :global     t

  (if other-frame-window-mode
      ;; enable
      (progn
	;; We assume Emacs code calls pop-to-buffer when there is a good
	;; reason to put the buffer in another window, so we don't mess
	;; with the default actions, except to allow
	;; display-buffer-reuse-window to use a window in another frame;
	;; add (reusable-frames . visible) to display-buffer-base-action
	;; attributes alist.
	(let ((functions (car display-buffer-base-action))
	      (attrs (cdr display-buffer-base-action)))
	  (push '(reusable-frames . visible) attrs)
	  (setq display-buffer-base-action (cons functions attrs)))

	;; Change switch-to-buffer to use display-buffer
	(if (fboundp 'advice-add)
	    ;; Emacs 25
	    (advice-add 'switch-to-buffer :around #'ofw-switch-to-buffer-advice)
	  ;; Emacs 24
	  (ad-activate 'switch-to-buffer))

	;; Completing-read <tab> pops up a buffer listing completions;
	;; that should not respect or consume
	;; ofw-frame-window-prefix-arg. We advise
	;; temp-buffer-window-show-advice (used by completing-read) to
	;; handle additional similar cases.
	(if (fboundp 'advice-add)
	    (advice-add 'temp-buffer-window-show :around #'ofw-temp-window-advice)
	  (ad-activate 'temp-buffer-window-show))
	)

    ;; else disable
    (let ((functions (car display-buffer-base-action))
	  (attrs (cdr display-buffer-base-action)))
      (setq attrs (delq '(reusable-frames . visible) attrs))
      (setq display-buffer-base-action (cons functions attrs)))

    (advice-remove 'switch-to-buffer #'ofw-switch-to-buffer-advice)
    (advice-remove 'temp-buffer-window-show #'ofw-temp-buffer-window-show-advice)
    ))

(unless (fboundp 'display-buffer-use-some-frame)
  ;; in Emacs 25; define here for earlier

(defun display-buffer-use-some-frame (buffer alist)
  "Display BUFFER in an existing frame that meets a predicate
\(by default any frame other than the current frame).  If
successful, return the window used; otherwise return nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, avoid
raising the frame.

If ALIST has a non-nil `frame-predicate' entry, its value is a
function taking one argument (a frame), returning non-nil if the
frame is a candidate; this function replaces the default
predicate.

If ALIST has a non-nil `inhibit-same-window' entry, avoid using
the currently selected window (only useful with a frame-predicate
that allows the selected frame)."
  (let* ((predicate (or (cdr (assq 'frame-predicate alist))
                        (lambda (frame)
                          (and
                           (not (eq frame (selected-frame)))
                           (not (window-dedicated-p
                                 (or
                                  (get-lru-window frame)
                                  (frame-first-window frame)))))
                          )))
         (frame (car (filtered-frame-list predicate)))
         (window (and frame (get-lru-window frame nil (cdr (assq 'inhibit-same-window alist))))))
    (when window
      (prog1
          (window--display-buffer
           buffer window 'frame alist display-buffer-mark-dedicated)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame frame))))
    ))
  )

(provide 'other-frame-window)
;; end of file
