;;; prog-proc.el --- Interacting from a source buffer with an inferior process   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 1999,2000,2003,2004,2005,2007,2012  Stefan Monnier
;; Copyright (C) 1994-1997  Matthew J. Morley
;; Copyright (C) 1989       Lars Bo Nielsen

;; ====================================================================

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 0139, USA.
;; (See sml-mode.el for HISTORY.) 

;; ====================================================================

;;; Commentary:

;; Prog-Proc is a package designed to complement Comint: while Comint was
;; designed originally to handle the needs of inferior process buffers, such
;; as a buffer running a Scheme repl, Comint does not actually provide any
;; functionality that links this process buffer with some source code.
;;
;; That's where Prog-Proc comes into play: it provides the usual commands and
;; key-bindings that lets the user send his code to the underlying repl.

;;; Code:

(eval-when-compile (require 'cl))
(require 'comint)
(require 'compile)

(defvar prog-proc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c ?\C-l] 'prog-proc-load-file)
    (define-key map [?\C-c ?\C-c] 'prog-proc-compile)
    (define-key map [?\C-c ?\C-z] 'prog-proc-switch-to)
    (define-key map [?\C-c ?\C-r] 'prog-proc-send-region)
    (define-key map [?\C-c ?\C-b] 'prog-proc-send-buffer)
    map)
  "Keymap for `prog-proc-mode'.")

(defvar prog-proc--buffer nil
  "The inferior-process buffer to which to send code.")
(make-variable-buffer-local 'prog-proc--buffer)

(defstruct (prog-proc-functions
            (:constructor prog-proc-make)
            (:predicate nil)
            (:copier nil))
  (name :read-only t)
  (run :read-only t)
  (load-cmd :read-only t)
  (chdir-cmd :read-only t)
  (compile-commands-alist :read-only t))

(defvar prog-proc-functions nil
  "Struct containing the various functions to create a new process, ...")

(defmacro prog-proc--prop (prop)
  `(,(intern (format "prog-proc-functions-%s" prop))
    (or prog-proc-functions
      ;; FIXME: Look for available ones and pick one.
        (error "Not an `prog-proc' buffer"))))
(defmacro prog-proc--call (method &rest args)
  `(funcall (prog-proc--prop ,method) ,@args))

;; The inferior process and his buffer are basically interchangeable.
;; Currently the code takes prog-proc--buffer as the main reference,
;; but all users should either use prog-proc-proc or prog-proc-buffer
;; to find the info.

(defun prog-proc-proc ()
  "Return the inferior process for the code in current buffer."
  (or (and (buffer-live-p prog-proc--buffer)
           (get-buffer-process prog-proc--buffer))
      (when (derived-mode-p 'prog-proc-mode 'prog-proc-comint-mode)
        (setq prog-proc--buffer (current-buffer))
        (get-buffer-process prog-proc--buffer))
      (let ((buf (prog-proc--call run)))
        (setq prog-proc--buffer buf)
        (get-buffer-process prog-proc--buffer))))

(defun prog-proc-buffer ()
  "Return the buffer of the inferior process."
  (process-buffer (prog-proc-proc)))

(defun prog-proc-switch-to ()
  "Switch to the buffer running the read-eval-print process."
  (pop-to-buffer (prog-proc-buffer)))

(defun prog-proc-send-string (proc str)
  (with-current-buffer (process-buffer proc)
    ;; FIXME: comint-send-string does not pass the string through
    ;; comint-input-filter-function, so we have to do it by hand.
    ;; Maybe we should insert the command into the buffer and then call
    ;; comint-send-input?
    (prog-proc-comint-input-filter-function nil)
    (comint-send-string proc (concat str "\n"))))
    
(defun prog-proc-load-file (file &optional and-go)
  "Load FILE into the read-eval-print process.
FILE is the file visited by the current buffer.
If prefix argument AND-GO is used, then we additionally switch
to the buffer where the process is running."
  (interactive
   (list (or buffer-file-name
             (read-file-name "File to load: " nil nil t))
         current-prefix-arg))
  (comint-check-source file)
  (let ((proc (prog-proc-proc)))
    (prog-proc-send-string proc (prog-proc--call load-cmd file))
    (when and-go (pop-to-buffer (process-buffer proc)))))

(defvar prog-proc--tmp-file nil)

(defun prog-proc-send-region (start end &optional and-go)
  "Send the content of the region to the read-eval-print process.
START..END delimit the region; AND-GO if non-nil indicate to additionally
switch to the process's buffer."
  (interactive "r\nP")
  (if (> start end) (let ((tmp end)) (setq end start) (setq start tmp))
    (if (= start end) (error "Nothing to send: the region is empty")))
  (let ((proc (prog-proc-proc))
        (tmp (make-temp-file "emacs-region")))
    (write-region start end tmp nil 'silently)
    (when prog-proc--tmp-file
      (ignore-errors (delete-file (car prog-proc--tmp-file)))
      (set-marker (cdr prog-proc--tmp-file) nil))
    (setq prog-proc--tmp-file (cons tmp (copy-marker start)))
    (prog-proc-send-string proc (prog-proc--call load-cmd tmp))
    (when and-go (pop-to-buffer (process-buffer proc)))))

(defun prog-proc-send-buffer (&optional and-go)
  "Send the content of the current buffer to the read-eval-print process.
AND-GO if non-nil indicate to additionally switch to the process's buffer."
  (interactive "P")
  (prog-proc-send-region (point-min) (point-max) and-go))

;; FIXME: How 'bout a menu?  Now, that's trickier because keymap inheritance
;; doesn't play nicely with menus!

(define-derived-mode prog-proc-mode prog-mode "Prog-Proc"
  "Major mode for editing source code and interact with an interactive loop."
  )

;;; Extended comint-mode for Prog-Proc.

(defun prog-proc-chdir (dir)
  "Change the working directory of the inferior process."
  (interactive "DChange to directory: ")
  (let ((dir (expand-file-name dir))
        (proc (prog-proc-proc)))
    (with-current-buffer (process-buffer proc)
      (prog-proc-send-string proc (prog-proc--call chdir-cmd dir))
      (setq default-directory (file-name-as-directory dir)))))

(defun prog-proc-comint-input-filter-function (str)
  ;; `compile.el' doesn't know that file location info from errors should be
  ;; recomputed afresh (without using stale info from earlier compilations).
  (compilation-forget-errors)       ;Has to run before compilation-fake-loc.
  (if prog-proc--tmp-file
      (compilation-fake-loc (cdr prog-proc--tmp-file)
                            (car prog-proc--tmp-file)))
  str)

(define-derived-mode prog-proc-comint-mode comint-mode "Prog-Proc-Comint"
  "Major mode for an inferior process used to run&compile source code."
  ;; Enable compilation-minor-mode, but only after the child mode is setup
  ;; since the child-mode might want to add rules to
  ;; compilation-error-regexp-alist.
  (add-hook 'after-change-major-mode-hook #'compilation-minor-mode nil t)
  ;; The keymap of compilation-minor-mode is too unbearable, so we
  ;; just can't use the minor-mode if we can't override the map.
  ;; Eliminate compilation-minor-mode's map.
  (let ((map (make-sparse-keymap)))
    (dolist (keys '([menu-bar] [follow-link]))
      ;; Preserve some of the bindings.
      (define-key map keys (lookup-key compilation-minor-mode-map keys)))
    (add-to-list 'minor-mode-overriding-map-alist
                 (cons 'compilation-minor-mode map)))

  (add-hook 'comint-input-filter-functions
            #'prog-proc-comint-input-filter-function nil t))

(defvar prog-proc-compile-command nil
  "The command used by default by `prog-proc-compile'.
See also `prog-proc-compile-commands-alist'.")

(defvar prog-proc-compile-commands-alist nil
  "Commands used by default by `prog-proc-compile'.
Each command is associated with its \"main\" file.
It is perfectly OK to associate several files with a command or several
commands with the same file.")

(defun prog-proc-compile (command &optional and-go)
  "Pass COMMAND to the read-eval-loop process to compile the current file.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil.  With prefix arg, always prompts.

Prefix arg AND-GO also means to switch to the read-eval-loop buffer afterwards."
  (interactive
   (let* ((dir default-directory)
	  (cmd "cd \"."))
     ;; Look for files to determine the default command.
     (while (and (stringp dir)
                 (progn
                   (dolist (cf (prog-proc--prop compile-commands-alist))
                     (when (file-exists-p (expand-file-name (cdr cf) dir))
                       (setq cmd (concat cmd "\"; " (car cf)))
                       (return nil)))
                   (not cmd)))
       (let ((newdir (file-name-directory (directory-file-name dir))))
	 (setq dir (unless (equal newdir dir) newdir))
	 (setq cmd (concat cmd "/.."))))
     (setq cmd
	   (cond
	    ((local-variable-p 'prog-proc-compile-command)
             prog-proc-compile-command)
	    ((string-match "^\\s-*cd\\s-+\"\\.\"\\s-*;\\s-*" cmd)
	     (substring cmd (match-end 0)))
	    ((string-match "^\\s-*cd\\s-+\"\\(\\./\\)" cmd)
	     (replace-match "" t t cmd 1))
	    ((string-match ";" cmd) cmd)
	    (t prog-proc-compile-command)))
     ;; code taken from compile.el
     (list (if (or compilation-read-command current-prefix-arg)
               (read-from-minibuffer "Compile command: "
				     cmd nil nil '(compile-history . 1))
             cmd))))
     ;; ;; now look for command's file to determine the directory
     ;; (setq dir default-directory)
     ;; (while (and (stringp dir)
     ;; 	    (dolist (cf (prog-proc--prop compile-commands-alist) t)
     ;; 	      (when (and (equal cmd (car cf))
     ;; 			 (file-exists-p (expand-file-name (cdr cf) dir)))
     ;; 		(return nil))))
     ;;   (let ((newdir (file-name-directory (directory-file-name dir))))
     ;;     (setq dir (unless (equal newdir dir) newdir))))
     ;; (setq dir (or dir default-directory))
     ;; (list cmd dir)))
  (set (make-local-variable 'prog-proc-compile-command) command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((dir default-directory))
    (when (string-match "^\\s-*cd\\s-+\"\\([^\"]+\\)\"\\s-*;" command)
      (setq dir (match-string 1 command))
      (setq command (replace-match "" t t command)))
    (setq dir (expand-file-name dir))
    (let ((proc (prog-proc-proc)))
      (with-current-buffer (process-buffer proc)
        (setq default-directory dir)
        (prog-proc-send-string
         proc (concat (prog-proc--call chdir-cmd dir) "\n" command))
        (when and-go (pop-to-buffer (process-buffer proc)))))))

(provide 'prog-proc)
;;; prog-proc.el ends here
