;;; sml-proc.el --- Comint based interaction mode for Standard ML.  -*- lexical-binding: t; coding: utf-8 -*-

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

;; FIXME-copyright.

;; Inferior-sml-mode is for interacting with an ML process run under
;; emacs. This uses the comint package so you get history, expansion,
;; backup and all the other benefits of comint. Interaction is
;; achieved by M-x run-sml which starts a sub-process under emacs. You may
;; need to set this up for autoloading in your .emacs:

;; (autoload 'run-sml "sml-proc" "Run an inferior ML process." t)

;; Exactly what process is governed by the variable sml-program-name
;; -- just "sml" by default. If you give a prefix argument (C-u M-x
;; run-sml) you will be prompted for a different program to execute from
;; the default -- if you just hit RETURN you get the default anyway --
;; along with the option to specify any command line arguments. Once
;; you select the ML program name in this manner, it remains the
;; default (unless you set in a hook, or otherwise).

;; NOTE: inferior-sml-mode-hook is run AFTER the ML program has been
;; launched. inferior-sml-load-hook is run only when sml-proc.el is
;; loaded into Emacs.

;; When running an ML process some further key-bindings are effective
;; in sml-mode buffer(s). C-c C-s (switch-to-sml) will split the
;; screen into two windows if necessary and place you in the ML
;; process buffer. In the interaction buffer, C-c C-s is bound to the
;; `sml' command by default (in case you need to restart).

;; C-c C-l (sml-load-file) will load an SML source file into the
;; inferior process, C-c C-r (sml-send-region) will send the current
;; region of text to the ML process, etc. Given a prefix argument to
;; these commands will switch you from the SML buffer to the ML
;; process buffer as well as sending the text. If you get errors
;; reported by the compiler, C-x ` (next-error) will step through
;; the errors with you.

;; NOTE. There is only limited support for this as it obviously
;; depends on the compiler's error messages being recognised by the
;; mode. Error reporting is currently only geared up for SML/NJ,
;; Moscow ML, and Poly/ML.  For other compilers, add the relevant
;; regexp to sml-error-regexp-alist and send it to me.

;; To send pieces of code to the underlying compiler, we never send the text
;; directly but use a temporary file instead.  This breaks if the compiler
;; does not understand `use', but has the benefit of allowing better error
;; reporting.

;; Bugs:

;; Todo:

;; - Keep improving `sml-compile'.

;;; Code:

(eval-when-compile (require 'cl))
(require 'sml-mode)
(require 'comint)
(require 'compile)

(defgroup sml-proc ()
  "Interacting with an SML process."
  :group 'sml)

(defcustom sml-compile-command "CM.make()"
  "The command used by default by `sml-compile'.
See also `sml-compile-commands-alist'.")

(defcustom sml-compile-commands-alist
  '(("CMB.make()" . "all-files.cm")
    ("CMB.make()" . "pathconfig")
    ("CM.make()" . "sources.cm")
    ("use \"load-all\"" . "load-all"))
  "Commands used by default by `sml-compile'.
Each command is associated with its \"main\" file.
It is perfectly OK to associate several files with a command or several
commands with the same file.")

(defvar inferior-sml-mode-hook nil
  "Hook is run when the inferior ML process is started.
All buffer local customisations for the interaction buffers go here.")


;;; ALL STUFF THAT DEFAULTS TO THE SML/NJ COMPILER (0.93)


;;; CODE

(defvar inferior-sml-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-c\C-s" 'run-sml)
    (define-key map "\C-c\C-l" 'sml-load-file)
    (define-key map "\t" 'completion-at-point)
    map)
  "Keymap for inferior-sml mode")

;; buffer-local

(defun sml-buffer (echo)
  "Make the current buffer the current `sml-buffer' if that is sensible.
Lookup variable `sml-buffer' to see why this might be useful.
If prefix argument ECHO is set, then it only reports on the current state."
  (interactive "P")
  (when (not echo)
    (setq sml-buffer
	  (if (eq major-mode 'inferior-sml-mode) (current-buffer)
	    (read-buffer "Set ML process buffer to: " nil t))))
  (message "ML process buffer is now %s."
	   (or (ignore-errors (buffer-name (get-buffer sml-buffer)))
	       "undefined")))

;;; FOR RUNNING ML FROM EMACS

;;;###autoload (autoload 'run-sml "sml-proc" nil t)
(defalias 'run-sml 'sml-run)

;; This is quite bogus, so it isn't bound to a key by default.
;; Anyone coming up with an algorithm to recognise fun & local
;; declarations surrounding point will do everyone a favour!

(defun sml-send-function (&optional and-go)
  "Send current paragraph to the inferior ML process. 
With a prefix argument AND-GO switch to the sml buffer as well 
\(cf. `sml-send-region'\)."
  (interactive "P")
  (save-excursion
    (sml-mark-function)
    (sml-send-region (point) (mark)))
  (if and-go (switch-to-sml nil)))

(defvar sml-source-modes '(sml-mode)
  "Used to determine if a buffer contains ML source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered an ML source file by `sml-load-file'.  Used by these commands
to determine defaults.")

;; Since sml-send-function/region take an optional prefix arg, these
;; commands are redundant. But they are kept around for the user to
;; bind if she wishes, since its easier to type C-c r than C-u C-c C-r.

(defun sml-send-region-and-go (start end)
  "Send current region START..END to the inferior ML process, and go there."
  (interactive "r")
  (sml-send-region start end t))

(defun sml-send-function-and-go ()
  "Send current paragraph to the inferior ML process, and go there."
  (interactive)
  (sml-send-function t))

;;; LOADING AND IMPORTING SOURCE FILES:

(defun sml-compile (command &optional and-go)
  "Pass a COMMAND to the SML process to compile the current program.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

Interactively, prompts for the command if `compilation-read-command' is
non-nil.  With prefix arg, always prompts.

Prefix arg AND-GO also means to `switch-to-sml' afterwards."
  (interactive
   (let* ((dir default-directory)
	  (cmd "cd \"."))
     ;; Look for files to determine the default command.
     (while (and (stringp dir)
		 (dolist (cf sml-compile-commands-alist 1)
		   (when (file-exists-p (expand-file-name (cdr cf) dir))
		     (setq cmd (concat cmd "\"; " (car cf))) (return nil))))
       (let ((newdir (file-name-directory (directory-file-name dir))))
	 (setq dir (unless (equal newdir dir) newdir))
	 (setq cmd (concat cmd "/.."))))
     (setq cmd
	   (cond
	    ((local-variable-p 'sml-compile-command) sml-compile-command)
	    ((string-match "^\\s-*cd\\s-+\"\\.\"\\s-*;\\s-*" cmd)
	     (substring cmd (match-end 0)))
	    ((string-match "^\\s-*cd\\s-+\"\\(\\./\\)" cmd)
	     (replace-match "" t t cmd 1))
	    ((string-match ";" cmd) cmd)
	    (t sml-compile-command)))
     ;; code taken from compile.el
     (if (or compilation-read-command current-prefix-arg)
	 (list (read-from-minibuffer "Compile command: "
				     cmd nil nil '(compile-history . 1)))
       (list cmd))))
     ;; ;; now look for command's file to determine the directory
     ;; (setq dir default-directory)
     ;; (while (and (stringp dir)
     ;; 	    (dolist (cf sml-compile-commands-alist t)
     ;; 	      (when (and (equal cmd (car cf))
     ;; 			 (file-exists-p (expand-file-name (cdr cf) dir)))
     ;; 		(return nil))))
     ;;   (let ((newdir (file-name-directory (directory-file-name dir))))
     ;;     (setq dir (unless (equal newdir dir) newdir))))
     ;; (setq dir (or dir default-directory))
     ;; (list cmd dir)))
  (set (make-local-variable 'sml-compile-command) command)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((dir default-directory))
    (when (string-match "^\\s-*cd\\s-+\"\\([^\"]+\\)\"\\s-*;" command)
      (setq dir (match-string 1 command))
      (setq command (replace-match "" t t command)))
    (setq dir (expand-file-name dir))
    (with-current-buffer (sml-proc-buffer)
      (setq default-directory dir)
      (sml-send-string (concat (format sml-cd-command dir) "; " command)
                       t and-go))))


(provide 'sml-proc)
;;; sml-proc.el ends here
