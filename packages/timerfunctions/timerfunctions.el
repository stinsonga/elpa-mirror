;;; timerfunctions.el --- Enhanced versions of some timer.el functions  -*- lexical-binding:t -*-

;; Copyright (C) 2000-2002, 2015-2016, 2020  Free Software Foundation, Inc.

;; Time-stamp: <2015-03-02 12:23:21 deego>
;; Emacs Lisp Archive entry
;; Filename: timerfunctions.el
;; Author: Dave Goel <deego3@gmail.com>
;; Version: 1.4.2
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
;; Created: 2000/11/20
;; Author's homepage: http://gnufans.net/~deego

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; For latest version:
;; Gives me a "Not found"!
;;(defconst timerfunctions-home-page
;;  "http://gnufans.net/~deego/emacspub/timerfunctions")

;;; Commentary:

;; See also: midnight.el (part of Emacs), timer.el

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar timerfunctions-version "1.4.2")


;;; New features:
(defconst timerfunctions-new-features
  "New since last posting: Changed the syntax of `tf-with-timeout' and
provided a `tf-with-timeout-check'.")

(defun timerfunctions-new-features ()
  "Provides electric help from variable `timerfunctions-new-features'."
  (interactive)
  (with-electric-help
   (lambda () (insert timerfunctions-new-features) nil) "*doc*"))


(defconst timerfunctions-introduction
  ;; FIXME: I think this belongs in the "Commentary:" instead.
  "timerfunctions.el contains some “enhanced” versions of a few timer.el
functions.  It is also used by vel.el, idledo.el etc.

 Suppose you want Emacs to run an action every REDOSECS for
 _as_long_as Emacs remains idle.  `tf-run-with-idle-timer' allows that.

 `tf-with-timeout' is a generalized with-timeout where you can inhibit
 breaks within parts of the body that you want.

 QUICKSTART:
 Place this file somewhere in your load-path, and add the
 following to your ~/.emacs: (load \"timerfunctions.el\")
"
)

;;;###autoload
(defun timerfunctions-introduction ()
  "Provides electric help from variable `timerfunctions-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert timerfunctions-introduction) nil) "*doc*"))

;;; Real Code:

;; FIXME: Why autoload?
;; FIXME: Why not use time-subtract?
;;;###autoload
(defun tf-time-difference (timeplus timesub)
  "Return the time in seconds elaspsed from TIMESUB to TIMEPLUS.

Conceptually:  \(- TIMEPLUS TIMESUB \)."
  (+ (* (expt 2 16) (- (car timeplus) (car timesub)))
     (- (cadr timeplus) (cadr timesub)))
)


(defun tf--get-secs (secs)
  (cond
   ((functionp secs) (funcall secs))
   ((numberp secs) secs)
   (t (eval secs t)))) ;; Obsolete calling convention!

;;;###autoload
(defun tf-run-with-idle-timer  (secs repeat redosecs redorepeat includeruntime function &rest args)
  "Similar to `run-with-idle-timer', except that provides more options.

Args are SECS, REPEAT, REDOSECS, REDOREPEAT, INCLUDERUNTIME,
FUNCTION and &rest ARGS.

Similar to `run-with-idle-timer', but provides more options.
Suppose you want Emacs to run an action every REDOSECS for as
long as Emacs remains idle.  Emacs' `run-with-idle-timer' will
perform the action exactly once every time Emacs goes idle.  This
function, on the other hand, will allow
you to keep performing an action as long as Emacs remains idle.

SECS is the number of seconds to wait once Emacs has first gone
idle or a function that returns this number when called with no arguments.
Note that the way `run-with-idle-timer' is defined, SECS will
unfortunately be called immediately after you call this function, but
REDOSECS will be *every* time Emacs *remains* idle..yay..

If REDOREPEAT is non-nil, the action is repeated as long Emacs remains
idle.  REDOSECS is the number of additional seconds (after the action
has been done) to wait if Emacs remains idle before performing the
action again.  Again, redosecs does not have to be a number, it can be
a function which yields such a number...

If INCLUDERUNTIME is non-nil, REDOSECS is the number of
additional seconds to wait after the action has been invoked (not
finished).

If REPEAT is nonnil, the entire cycle is repeated every time Emacs
next goes idle.. (as in the default `run-with-idle-timer')."
  (apply #'run-with-idle-timer
	 (tf--get-secs secs) repeat #'tf--run-while-idle
	 redosecs redorepeat includeruntime
	 function args)
  )


(defun tf--run-while-idle (redosecs redorepeat includeruntime function &rest args)
  "A simplified version of `tf-run-with-idle-timer'.

Runs FUNCTION with ARGS and optionally repeats if Emacs remains idle.
Probably is of no use unless used in programs.
 If REDOREPEAT is non-nil, the function is repeated periodically every
REDOSECS as long as Emacs remains idle. By default, Emacs waits
REDOSECS *after* the function is done executing to repeat.  If you want
the execution-time to count towards REDOSECS, make INCLUDERUNTIME
non-nil.
REDOSECS can be a number or a function that returns a number."
  (if (not includeruntime)
      (progn
	(apply function args)
	(if redorepeat
	    (while (sit-for (tf--get-secs redosecs))
	      (apply function args))))
    (progn
      (let ((before-time (current-time)))
	(apply function args)
	(if redorepeat
	    (while (sit-for (-
			     (tf--get-secs redosecs)
			     (tf-time-difference (current-time)
						 before-time)))
	      (setq before-time (current-time))
	      (apply function args))))))
  )




(defun tf-todo-wait-until-idle (&optional secs)
  ;; FIXME: This can only work if/when we add concurrency to Elisp, IIUC.
  "This function is not functional yet.

Waits until idle.  Arguments are SECS.
Will help run processes in background.  This function will NOT create
a timer.  Will simply use `sit-for'."
  (if (null secs)
      (setq secs 1))
  (while (not (sit-for secs))
    (sit-for 1))
  (message "tf-todo-wait-until-idle DONE WAITING!")
)


;;;Tue Jan 23 17:38:44 2001
;; FIXME: Use `with-demoted-errors' instead.
(defmacro tf-ignore-errors (&rest body)
 "Ignore errors in BODY, but loudly."
 (let ((err (make-symbol "err")))
   `(condition-case ,err (progn ,@body)
      (error (message "IGNORED ERROR: %s" (error-message-string ,err))))))


(defvar tf-with-timeout-repeat-sec 0.01
  "Interval between checks for inhibitedness.

If the initial timeout fails because of inhibitedness, we shall
check every `tf-with-timeout-repeat-sec' seconds to see if we are
uninhibited, yet.  This variable is customizable.")


(defun tf--with-timeout-handler-internal (tag timedoutvar inhibit-var)
  "Internal function."
  (set timedoutvar t)
  ;;(tf-with-timeout-check tag timedoutvar inhibitp)
  ;; which is equivalent to:
  (unless (symbol-value inhibit-var)
    (tf-ignore-errors (throw tag 'timeout)))
  )

(defun tf-with-timeout-check (inhibit-var tag timedoutvar)
  "Internal function.
Check whether timeout has actually reached.
We need this step because this function might be called by the
user as well."
  (when (symbol-value timedoutvar)
    (unless (symbol-value inhibit-var)
      (tf-ignore-errors (throw tag 'timeout)))))



(defvar tf-tag-tmpvar nil)

(defmacro tf-catch (tag &rest body)     ;FIXME: Unused.
  "Catch a TAG in BODY."
  (declare (obsolete catch "1.4.3"))
  `(let
       ;; unquote the tag here..
       ((,(cadr tag) 'tf-catch))
     (catch ,tag
       ,@body)))

(defun tf-throw (tag value)             ;FIXME: Unused.
  "Throw a TAG with value VALUE."
  (declare (obsolete throw "1.4.3"))
  (when (eql (eval tag t) 'tf-catch)
     (throw tag value)))

;;;###autoload
(defmacro tf-with-timeout (inhibit-var timertag timedoutvar tlist &rest body)
  "Like `with-timeout' but with support for unbreakable code.

Provides ability to inhibit timeout during parts of the body.
Note that most of the time, you may not need this functionality
at all unless you want to be very “clean” about things---you
could get by with the regular with-timeout and not using
sit-for's in the body.  Or with the regular with-timeout and
using unwind-protect.


A judicious use of `unwind-protect' may seem to alleviate the
need for this function. This function, however, provides
additional flexibility in that the inhibitedness can be altered
at run-time depending on various conditions.


Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS which are contained in TLIST
and return the value of the last one.
The call should look like:
 (tf-with-timeout quoted-expr (SECONDS TIMEOUT-FORMS...) BODY...)

The timeout is checked whenever Emacs waits for some kind of external
event \(such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected.  Furthermore:

During the execution of the body, we SHALL NOT time out when INHIBIT-VAR's
content is non-nil.  Thus, for example, you might initially setq a
variable my-var as nil, supply inhibitp as `my-var', and then you may
setq my-var to t or nil within the body of tf-with-timeout to enable
or disable timeout.  The best use of this functionality is to setq
inhibitp to t when during parts of loops where you do not want the
body broken within certain parts of the loop.  (Of course, if that
part of the loop does not contain any sit-for's or read's then you
don't have to worry about this in the first place..)


Again, Do not forget to bind my-var to some value before attempting to use this
tf-with-timeout :)

Here's an example:


 (let ((myinhibit t))
  (tf-with-timeout \\='myinhibit \\='mytag \\='mytimedoutvar
		   (2 2)
		   (setq a nil)
		   (setq b nil)
		   (sit-for 4)
		   (setq a 4)
		   (setq myinhibit nil)
		   (sit-for 2)
		   (setq b 5)
		   ))


The above example requests a timeout within 2 seconds.  However, the
timeout can takes place only when myinhibit is set to nil,
which becomes true after about 4 seconds.  Thus, after the execution of the
body, a has the value 4, but b has the value nil.

See `tf-test-timeout' for another example.

Important Note: If the body of a loop tends to stay in a timeout
inhibited region for most of the time, then make sure that the timeout
enabled region atleast spans about 0.02 seconds.. thus, use (sleep-for
0.02) if needed.. this is because we check every 0.01 seconds if an
uninhibited timeout condition has been satisfied.

But perhaps you do not want to include (sleep-for 0.02) because that
wastes precious cpu time.  Simple, don't include it, just after a long
inhibited body, you can include a timeout check within the body
instead of (sleep-for 0.02):
 (tf-with-timeout-check \\='mytag \\='mytimedoutvar \\='myinhibitp)

Moreover, if that is the main check you rely on, you it perhaps makes
sense to increase the value of `tf-with-timeout-repeat-sec', so that
your cpu cycles are not wasted every 0.01 sec.  See the doc of that
variable for more.

TIMERTAG should be a quoted symbol, also we WILL set that symbol to t
during the execution of these forms.

TIMEDOUTVAR is the variable that times out."
  (let ((seconds (car tlist))
	(timeout-forms (cdr tlist)))
    `(let (
	   ;;(with-timeout-tag (cons nil nil))
	   with-timeout-value with-timeout-timer)
       (set ,timedoutvar nil)          ;FIXME: Why do we need this?
       (if (catch ,timertag
	     (progn
	       (setq with-timeout-timer
		     (run-with-timer ,seconds tf-with-timeout-repeat-sec
				     #'tf--with-timeout-handler-internal
				     ,timertag ,timedoutvar
				     ,inhibit-var))
	       (setq with-timeout-value (progn ,@body))
	       nil))
	   (progn ,@timeout-forms)
	 (cancel-timer with-timeout-timer)
	 with-timeout-value))))


;;;====================================================
;;;TESTS FOLLOW
(defun tf-test-display-time-internal ()
  "A test function."
  (interactive)
  (let ((thisbuffer (buffer-name)))
    (switch-to-buffer-other-window "*scratch*")
    (goto-char (point-max))
    (insert (concat "\n" (format "%S" (cadr (current-time)))))
    (recenter)
    (switch-to-buffer-other-window thisbuffer))
)


(defun tf-test-idle-timer ()
  "A test function.

Run this and watch Play around with the options.  If you run it,
you may have to exit your Emacs session to restore normal Emacs,
unless you clean things up carefully!"

  (interactive)
  (tf-run-with-idle-timer
  1 t 3 t nil #'tf-test-display-time-internal)
)





(defun tf-test-timeout ()
  "Bad count should be zero."
  (interactive)
  (defvar tf--test-inhi) (defvar tf--test-myvar)
  (let ((tf--test-inhi nil) (goodcount 0) (badcount 0) (a 1) (b 2)
	(tf--test-myvar nil)
	)
    (cl-loop
     for ctr from 0 to 10 do
     (message "ctr=%S" ctr)
     (tf-with-timeout 'tf--test-inhi 'mytag 'tf--test-myvar
      (0.3 nil)
      (cl-loop for i from 0 to 100000 do
	    (message "ctr=%S, i=%S" ctr i)
	    (setq tf--test-inhi t)
	    (setq a (random 100))
	    (sleep-for 0.1)
	    (setq b a)
	    (setq tf--test-inhi nil)
	    (sleep-for 0.02)
	    ))
     (if (equal b a) (cl-incf goodcount) (cl-incf badcount)))
    (message "Goodcount: %S; badcount: %S" goodcount badcount)))



(defun tf-test-timeout-complex ()
  "Should return a value of 20000 for a."

  (interactive)
  (defvar tf--test-inhi) (defvar tf--test-myvar)
  (let ((tf--test-inhi t) (ctr 0) (a 1)
	(tf--test-myvar nil)
	)
    (setq a 0)
    (message "ctr=%S" ctr)
    (tf-with-timeout
     'tf--test-inhi 'mytag 'tf--test-myvar
     (0.1 nil)
     (cl-loop for i from 0 to 10000 do
	   (message "first loop. ctr=%S, i=%S, " ctr i)
	   (cl-incf a))
     (message "initial loop ends here.")
     ;; no throw here because loop prohibited.
     (tf-with-timeout-check 'tf--test-inhi 'mytag 'tf--test-myvar)
     ;; this shouldn't help either
     (sit-for 0.3)

     (cl-loop for i from 0 to 10000 do
	   (message "second loop.  i=%S" i)
	   (cl-incf a))
     (message "second loop ends here.")
     (setq tf--test-inhi nil)
     ;; this should throw.
     (tf-with-timeout-check 'tf--test-inhi 'mytag 'tf--test-myvar)
     ;; this should NOT be needed.
     ;;(sit-for 0.2)
     ;; this loop should never take place.
     (cl-loop for i from 0 to 1000 do
	   (message "third loop, i=%S" i)
	   (cl-incf a))
     (message "third loop ends here."))
    (message "%S" a)
    a))



(defvar tf--internal-var-recenter 1)

(defun tf-test-internal-recenter-toggle ()
  "A test function."
  (interactive)
  (recenter 1)
  (setq tf--internal-var-recenter (- 0 tf--internal-var-recenter)))

(defun tf-test-example-timer-recenter ()
  "An example timer.
Changes the screen display every 3 seconds, thus ensuring that you
don't time out of ssh sessions."
  (interactive)
  (tf-run-with-idle-timer 3 t 3 t nil #'tf-test-internal-recenter-toggle))


(provide 'timerfunctions)
;;; timerfunctions.el ends here
