;;; Copyright (C) 2010, 2012-2015 Rocky Bernstein <rocky@gnu.org>
;;; Python "trepan3k" Debugger tracking a comint buffer.

(eval-when-compile (require 'cl))
(require 'load-relative)
(require-relative-list '(
			 "../../common/cmds"
			 "../../common/menu"
			 "../../common/track"
			 "../../common/track-mode"
			 )
		       "realgud-")
(require-relative-list '("core" "init") "realgud:trepan3k-")
(require-relative-list '("../../lang/python") "realgud-lang-")

(declare-function realgud-track-mode 'realgud-track-mode)
(declare-function realgud-track-mode-hook 'realgud-track-mode)
(declare-function realgud-track-mode-setup 'realgud-track-mode)
(declare-function realgud:track-set-debugger 'realgud-track-mode)
(declare-function realgud-python-populate-command-keys 'realgud-lang-python)

(realgud-track-mode-vars "trepan3k")

(declare-function realgud-track-mode(bool))

(realgud-python-populate-command-keys trepan3k-track-mode-map)

(defun trepan3k-track-mode-hook()
  (if trepan3k-track-mode
      (progn
	(use-local-map trepan3k-track-mode-map)
	(message "using trepan3k mode map")
	)
    (message "trepan3k track-mode-hook disable called")
    )
)

(define-minor-mode trepan3k-track-mode
  "Minor mode for tracking trepan3k source locations inside a process shell via realgud. trepan3k is a Python debugger. See URL `http://code.google.com/p/python3-trepan/'.

If called interactively with no prefix argument, the mode is toggled. A prefix argument, captured as ARG, enables the mode if the argument is positive, and disables it otherwise.

\\{trepan3k-track-mode-map}
"
  :init-value nil
  ;; :lighter " trepan3k"   ;; mode-line indicator from realgud-track is sufficient.
  ;; The minor mode bindings.
  :global nil
  :group 'realgud:trepan3k
  :keymap trepan3k-track-mode-map
  (realgud:track-set-debugger "trepan3k")
  (if trepan3k-track-mode
      (progn
	(realgud-track-mode-setup 't)
	(trepan3k-track-mode-hook))
    (progn
      (setq realgud-track-mode nil)
      ))
)

(define-key trepan3k-short-key-mode-map "T" 'realgud:cmd-backtrace)

(provide-me "realgud:trepan3k-")
