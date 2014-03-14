;;; gnugo.el --- play GNU Go in a buffer

;; Copyright (C) 2014  Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Version: 2.3.1

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

;; Playing
;; -------
;;
;; This file provides the command `gnugo' which allows you to play the game of
;; go against the external program "gnugo" (http://www.gnu.org/software/gnugo)
;; in a dedicated Emacs buffer, or to resume a game in progress.  NOTE: In
;; this file, to avoid confusion w/ elisp vars and funcs, we use the term "GNU
;; Go" to refer to the process object created by running the external program.
;;
;; At the start of a new game, you can pass additional command-line arguments
;; to GNU Go to specify level, board size, color, komi, handicap, etc.  By
;; default GNU Go plays at level 10, board size 19, color white, and zero for
;; both komi and handicap.
;;
;; To play a stone, move the cursor to the desired vertice and type `SPC' or
;; `RET'; to pass, `P' (note: uppercase); to quit, `q'; to undo one of your
;; moves (as well as a possibly intervening move by GNU Go), `u'.  To undo
;; back through an arbitrary stone that you played, place the cursor on a
;; stone and type `U' (note: uppercase).  Other keybindings are described in
;; the `gnugo-board-mode' documentation, which you may view with the command
;; `describe-mode' (normally `C-h m') in that buffer.  The buffer name shows
;; the last move and who is currently to play.  Capture counts and other info
;; are shown on the mode line immediately following the major mode name.
;;
;; While GNU Go is pondering its next move, certain commands that rely on its
;; assistence will result in a "still waiting" error.  Do not be alarmed; that
;; is normal.  When it is your turn again you may retry the command.  In the
;; meantime, you can use Emacs for other tasks, or start an entirely new game
;; with `C-u M-x gnugo'.  (NOTE: A new game will slow down all games. :-)
;;
;; If GNU Go should crash during a game the mode line will show "no process".
;; Please report the event to the GNU Go maintainers so that they can improve
;; the program.
;;
;;
;; Meta-Playing (aka Customizing)
;; ------------------------------
;;
;; Customization is presently limited to
;;   vars:            `gnugo-program'
;;                    `gnugo-animation-string'
;;                    `gnugo-mode-line'
;;                    `gnugo-X-face' `gnugo-O-face' `gnugo-grid-face'
;;                    `gnugo-xpms'
;;   normal hooks:    `gnugo-board-mode-hook'
;;                    `gnugo-post-move-hook'
;;   and the keymap:  `gnugo-board-mode-map'
;;
;; The variable `gnugo-xpms' is a special case.  To set it you need to load
;; gnugo-xpms.el (http://www.emacswiki.org) or some other library w/ congruent
;; interface.

;;; Code:

(eval-when-compile (require 'cl))       ; use the source luke!
(require 'time-date)                    ; for `time-subtract'

;;;---------------------------------------------------------------------------
;;; Political arts

(defconst gnugo-version "2.3.1"
  "Version of gnugo.el currently loaded.
This follows a MAJOR.MINOR.PATCH scheme.")

;;;---------------------------------------------------------------------------
;;; Variables for the uninquisitive programmer

(defvar gnugo-program "gnugo"
  "Command to start an external program that speaks GTP, such as \"gnugo\".
The value may also be in the form \"PROGRAM OPTIONS...\" in which case the
the command `gnugo' will prefix OPTIONS in its default offering when it
queries you for additional options.  It is an error for \"--mode\" to appear
in OPTIONS.

For more information on GTP and GNU Go, feel free to visit:
http://www.gnu.org/software/gnugo")

(defvar gnugo-board-mode-map nil
  "Keymap for GNUGO Board mode.")

(defvar gnugo-board-mode-hook nil
  "Hook run when entering GNUGO Board mode.")

(defvar gnugo-inhibit-refresh nil
  "Used in `gnugo-post-move-hook'.")

(defvar gnugo-post-move-hook nil
  "Normal hook run after a move and before the board is refreshed.
Hook functions can prevent the call to `gnugo-refresh' by evaluating:
  (setq gnugo-inhibit-refresh t)
Initially, when `run-hooks' is called, the current buffer is the GNUGO
Board buffer of the game.  Hook functions that switch buffers must take
care not to call (directly or indirectly through some other function)
`gnugo-put' or `gnugo-get' after the switch.")

(defvar gnugo-animation-string
  (let ((jam "*#") (blink " #") (spin "-\\|/") (yada "*-*!"))
    (concat jam jam jam jam jam
            ;; "SECRET MESSAGE HERE"
            blink blink blink blink blink blink blink blink
            ;; Playing go is like fighting ignorance: when you think you have
            ;; surrounded something by knowing it very well it often turns
            ;; out that in the time you spent deepening this understanding,
            ;; other areas of ignorance have surrounded you.
            spin spin spin spin spin spin spin spin spin
            ;; Playing go is not like fighting ignorance: what one person
            ;; knows many people may come to know; knowledge does not build
            ;; solely move by move.  Wisdom, on the other hand...
            yada yada yada))
  "String whose individual characters are used for animation.
Specifically, the commands `gnugo-worm-stones' and `gnugo-dragon-stones'
render the stones in their respective result groups as the first character
in the string, then the next, and so on.")

(defvar gnugo-mode-line "~b ~w :~m :~u"
  "A `mode-line-format'-compliant value for GNUGO Board mode.
If a single string, the following special escape sequences are
replaced with their associated information:
  ~b,~w  black,white captures (a number)
  ~p     current player (black or white)
  ~m     move number
  ~t     time waiting for the current move
  ~u     time taken for the Ultimate (most recent) move
The times are in seconds, or \"-\" if that information is not available.
For ~t, the value is a snapshot, use `gnugo-refresh' to update it.")

(defvar gnugo-X-face 'font-lock-string-face
  "Name of face to use for X (black) stones.")

(defvar gnugo-O-face 'font-lock-builtin-face
  "Name of face to use for O (white) stones.")

(defvar gnugo-grid-face 'default
  "Name of face to use for the grid (A B C ... 1 2 3 ...).")

;;;---------------------------------------------------------------------------
;;; Variables for the inquisitive programmer

(defconst gnugo-font-lock-keywords
  '(("X" . gnugo-X-face)
    ("O" . gnugo-O-face))
  "Font lock keywords for `gnugo-board-mode'.")

(defvar gnugo-option-history nil)

(defvar gnugo-state nil)                ; hint: C-c C-p

(eval-when-compile
  (defvar gnugo-xpms nil))

;;;---------------------------------------------------------------------------
;;; Support functions

(defsubst gnugo--compare-strings (s1 beg1 s2 beg2)
  (compare-strings s1 beg1 nil s2 beg2 nil))

(defun gnugo-put (key value)
  "Associate move/game/board-specific property KEY with VALUE.

There are many properties, each named by a keyword, that record and control
how gnugo.el manages each game.  Each GNUGO Board buffer has its own set
of properties, stored in the hash table `gnugo-state'.  Here we document
some of the more stable properties.  You may wish to use them as part of
a `gnugo-post-move-hook' function, for example.  Be careful to preserve
the current buffer as `gnugo-state' is made into a buffer-local variable.
NOTE: In the following, \"see foo\" actually means \"see foo source or
you may never really understand to any degree of personal satisfaction\".

 :proc -- subprocess named \"gnugo\", \"gnugo<1>\" and so forth

 :diamond -- the part of the subprocess name after \"gnugo\", may be \"\"

 :game-over -- nil until game over at which time its value is set to
               the alist `((live GROUP ...) (dead GROUP ...))'

 :sgf-collection -- after a `loadsgf' command, entire parse tree of file,
                    a simple list of one or more gametrees, updated in
                    conjunction w/ :sgf-gametree and :monkey

 :sgf-gametree -- one of the gametrees in :sgf-collection

 :monkey -- vector of three elements: LOC, a pointer to a node on the
            :sgf-gametree representing the most recently-played move
            (the next move modifies the cdr of LOC); MEM, the simple
            reverse-chronological list of previous LOC pointers; and
            COUNT, the number of moves from the beginning of the game

 :gnugo-color -- either \"black\" or \"white\"
 :user-color
 :last-mover

 :last-waiting  -- seconds and time value, respectively; see `gnugo-push-move'
 :waiting-start

 :black-captures -- these are strings since gnugo.el doesn't do anything
 :white-captures    w/ the information besides display it in the mode line;
                    gory details in functions `gnugo-propertize-board-buffer'
                    and `gnugo-merge-showboard-results' (almost more effort
                    than they are worth!)

 :display-using-images -- XPMs, to be precise; see functions `gnugo-yy',
                          `gnugo-toggle-image-display' and `gnugo-refresh',
                          as well as gnugo-xpms.el (available elsewhere)

 :all-yy -- list of 46 keywords used as the `category' text property
            (so that their plists, typically w/ property `display' or
            `do-not-display') are consulted by the Emacs display engine;
            46 = 9 places * (4 moku + 1 empty) + 1 hoshi; see functions
            `gnugo-toggle-image-display', `gnugo-yy' and `gnugo-yang'

 :lparen-ov -- overlays shuffled about to indicate the last move; only
 :rparen-ov    one is used when displaying using images

 :last-user-bpos -- board position; keep the hapless human happy

As things stabilize probably more info will be added to this docstring."
  (declare (indent 1))
  (puthash key value gnugo-state))

(defun gnugo-get (key)
  "Return the move/game/board-specific value for KEY.
See `gnugo-put'."
  (gethash key gnugo-state))

(defun gnugo-describe-internal-properties ()
  "Pretty-print `gnugo-state' properties in another buffer.
Handle the big, slow-to-render, and/or uninteresting ones specially."
  (interactive)
  (let ((buf (current-buffer))
        (d (gnugo-get :diamond))
        (acc (loop for key being the hash-keys of gnugo-state
                   using (hash-values val)
                   collect (cons key
                                 (case key
                                   ((:xpms :local-xpms)
                                    (format "hash: %X (%d images)"
                                            (sxhash val)
                                            (length val)))
                                   (:sgf-collection
                                    (length val))
                                   (:monkey
                                    (let ((loc (aref val 0)))
                                      (list (length (aref val 1))
                                            (length (cdr loc))
                                            (car loc))))
                                   (t val))))))
    (switch-to-buffer (get-buffer-create
                       (format "%s*GNUGO Board Properties*"
                               (gnugo-get :diamond))))
    (erase-buffer)
    (emacs-lisp-mode)
    (setq truncate-lines t)
    (save-excursion
      (let ((standard-output (current-buffer)))
        (pp (reverse acc)))
      (goto-char (point-min))
      (let ((rx (format "overlay from \\([0-9]+\\).+\n%s\\s-+"
                        (if (string= "" d)
                            ".+\n"
                          ""))))
        (while (re-search-forward rx (point-max) t)
          (let ((pos (get-text-property (string-to-number (match-string 1))
                                        'gnugo-position buf)))
            (delete-region (+ 2 (match-beginning 0)) (point))
            (insert (format " %S" pos))))))
    (message "%d properties" (length acc))))

(defun gnugo-board-buffer-p (&optional buffer)
  "Return non-nil if BUFFER is a GNUGO Board buffer."
  (with-current-buffer (or buffer (current-buffer)) gnugo-state))

(defun gnugo-board-user-play-ok-p (&optional buffer)
  "Return non-nil if BUFFER is a GNUGO Board buffer ready for a user move."
  (with-current-buffer (or buffer (current-buffer))
    (and gnugo-state (not (gnugo-get :waitingp)))))

(defun gnugo-other (color)
  (if (string= "black" color) "white" "black"))

(defun gnugo-gate (&optional in-progress-p)
  (unless (gnugo-board-buffer-p)
    (user-error "Wrong buffer -- try M-x gnugo"))
  (unless (gnugo-get :proc)
    (user-error "No \"gnugo\" process!"))
  (when (gnugo-get :waitingp)
    (user-error "Not your turn yet -- please wait for \"\(%s to play\)\""
                (gnugo-get :user-color)))
  (when (and (gnugo-get :game-over) in-progress-p)
    (user-error "Sorry, game over")))

(defun gnugo-sentinel (proc string)
  (let ((status (process-status proc)))
    (when (memq status '(exit signal))
      (let ((buf (process-buffer proc)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (setq mode-line-process
                  (list " [%s ("
                        (propertize (car (split-string string))
                                    'face 'font-lock-warning-face)
                        ")]"))
            (when (eq proc (gnugo-get :proc))
              (gnugo-put :proc nil))))))))

(defun gnugo--begin-exchange (proc filter line)
  (declare (indent 2))                  ; good time, for a rime
                                        ; nice style, for a wile...
  (set-process-filter proc filter)
  (process-send-string proc line)
  (process-send-string proc "\n"))

(defun gnugo--q (fmt &rest args)
  "Send formatted command \"FMT ARGS...\"; wait for / return response.
The response is a string whose first two characters indicate the
status of the command.  See also `gnugo-query'."
  (when (gnugo-get :waitingp)
    (user-error "Sorry, still waiting for %s to play"
                (gnugo-get :gnugo-color)))
  (let ((proc (gnugo-get :proc)))
    (process-put proc :incomplete t)
    (process-put proc :srs "")          ; synchronous return stash
    (gnugo--begin-exchange
        proc (lambda (proc string)
               (let ((full (concat (process-get proc :srs)
                                   string)))
                 (process-put proc :srs full)
                 (unless (numberp (gnugo--compare-strings
                                   full (max 0 (- (length full)
                                                  2))
                                   "\n\n" nil))
                   (process-put proc :incomplete nil))))
      (if (null args)
          fmt
        (apply #'format fmt args)))
    (while (process-get proc :incomplete)
      (accept-process-output proc 30))
    (prog1 (substring (process-get proc :srs) 0 -2)
      (process-put proc :srs ""))))

(defun gnugo-query (message-format &rest args)
  "Send GNU Go a command formatted with MESSAGE-FORMAT and ARGS.
Return a string that omits the first two characters (corresponding
to the status indicator in the Go Text Protocol).  Use this function
when you are sure the command cannot fail."
  (substring (apply 'gnugo--q message-format args)
             2))

(defun gnugo-lsquery (message-format &rest args)
  (split-string (apply 'gnugo-query message-format args)))

(defsubst gnugo-treeroot (prop)
  (cdr (assq prop (car (gnugo-get :sgf-gametree)))))

(defun gnugo-goto-pos (pos)
  "Move point to board position POS, a letter-number string."
  (goto-char (point-min))
  (forward-line (- (1+ (gnugo-treeroot :SZ))
                   (string-to-number (substring pos 1))))
  (forward-char 1)
  (forward-char (+ (if (= 32 (following-char)) 1 2)
                   (* 2 (- (let ((letter (aref pos 0)))
                             (if (> ?I letter)
                                 letter
                               (1- letter)))
                           ?A)))))

(defun gnugo-f (frag)
  (intern (format ":gnugo-%s%s-props" (gnugo-get :diamond) frag)))

(defun gnugo-yang (c)
  (cdr (assq c '((?+ . hoshi)
                 (?. . empty)
                 (?X . (bmoku . bpmoku))
                 (?O . (wmoku . wpmoku))))))

(defun gnugo-yy (yin yang &optional momentaryp)
  (gnugo-f (format "%d-%s"
                   yin (cond ((and (consp yang) momentaryp) (cdr yang))
                             ((consp yang) (car yang))
                             (t yang)))))

(defun gnugo-toggle-image-display ()
  (unless (and (fboundp 'display-images-p) (display-images-p))
    (user-error "Display does not support images, sorry"))
  (require 'gnugo-xpms)
  (unless (and (boundp 'gnugo-xpms) gnugo-xpms)
    (user-error "Could not load `gnugo-xpms', sorry"))
  (let ((fresh (or (gnugo-get :local-xpms) gnugo-xpms)))
    (unless (eq fresh (gnugo-get :xpms))
      (gnugo-put :xpms fresh)
      (gnugo-put :all-yy nil)))
  (let* ((new (not (gnugo-get :display-using-images)))
         (act (if new 'display 'do-not-display)))
    (mapc (lambda (yy)
            (setcar (symbol-plist yy) act))
          (or (gnugo-get :all-yy)
              (gnugo-put :all-yy
                (prog1 (mapcar (lambda (ent)
                                 (let* ((k (car ent))
                                        (yy (gnugo-yy (cdr k) (car k))))
                                   (setplist yy `(not-yet ,(cdr ent)))
                                   yy))
                               (gnugo-get :xpms))
                  (let ((imul (image-size (get (gnugo-yy 5 (gnugo-yang ?+))
                                               'not-yet))))
                    (gnugo-put :w-imul (car imul))
                    (gnugo-put :h-imul (cdr imul)))))))
    (setplist (gnugo-f 'ispc) (and new '(display (space :width 0))))
    (gnugo-put :highlight-last-move-spec
      (if new
          '((lambda (p)
              (get (gnugo-yy (get-text-property p 'gnugo-yin)
                             (get-text-property p 'gnugo-yang)
                             t)
                   'display))
            0 delete-overlay)
        (gnugo-get :default-highlight-last-move-spec)))
    ;; a kludge to be reworked another time perhaps by another gnugo.el lover
    (dolist (group (cdr (assq 'dead (gnugo-get :game-over))))
      (mapc 'delete-overlay (cdar group))
      (setcdr (car group) nil))
    (gnugo-put :wmul (if new (gnugo-get :w-imul) 1))
    (gnugo-put :hmul (if new (gnugo-get :h-imul) 1))
    (gnugo-put :display-using-images new)))

(defun gnugo-toggle-grid ()
  "Turn the grid around the board on or off."
  (interactive)
  (funcall (if (memq :nogrid buffer-invisibility-spec)
               'remove-from-invisibility-spec
             'add-to-invisibility-spec)
           :nogrid)
  (save-excursion (gnugo-refresh)))

(defun gnugo-propertize-board-buffer ()
  (erase-buffer)
  (insert (substring (gnugo--q "showboard") 3))
  (let* ((grid-props (list 'invisible :nogrid
                           'font-lock-face gnugo-grid-face))
         (%gpad (gnugo-f 'gpad))
         (%gspc (gnugo-f 'gspc))
         (%lpad (gnugo-f 'lpad))
         (%rpad (gnugo-f 'rpad))
         (ispc-props (list 'category (gnugo-f 'ispc) 'rear-nonsticky t))
         (size (gnugo-treeroot :SZ))
         (size-string (number-to-string size)))
    (goto-char (point-min))
    (put-text-property (point) (1+ (point)) 'category (gnugo-f 'tpad))
    (skip-chars-forward " ")
    (put-text-property (1- (point)) (point) 'category %gpad)
    (put-text-property (point) (progn (end-of-line) (point)) 'category %gspc)
    (forward-char 1)
    (add-text-properties (1+ (point-min)) (1- (point)) grid-props)
    (while (looking-at "\\s-*\\([0-9]+\\)[ ]")
      (let* ((row (match-string-no-properties 1))
             (edge (match-end 0))
             (other-edge (+ edge (* 2 size) -1))
             (right-empty (+ other-edge (length row) 1))
             (top-p (string= size-string row))
             (bot-p (string= "1" row)))
        (let* ((nL (- edge 1 (length size-string)))
               (nR (- edge 1))
               (ov (make-overlay nL nR (current-buffer) t)))
          (add-text-properties nL nR grid-props)
          ;; We redundantly set `invisible' in the overlay to workaround
          ;; a display bug whereby text *following* the overlaid text is
          ;; displayed with the face of the overlaid text, but only when
          ;; that text is invisible (i.e., `:nogrid' in invisibility spec).
          ;; This has something to do w/ the bletcherous `before-string'.
          (overlay-put ov 'invisible :nogrid)
          (overlay-put ov 'category %lpad))
        (do ((p edge (+ 2 p)) (ival 'even (if (eq 'even ival) 'odd 'even)))
            ((< other-edge p))
          (let* ((position (format "%c%s" (aref "ABCDEFGHJKLMNOPQRST"
                                                (truncate (- p edge) 2))
                                   row))
                 (yin (let ((A-p (= edge p))
                            (Z-p (= (1- other-edge) p)))
                        (cond ((and top-p A-p) 1)
                              ((and top-p Z-p) 3)
                              ((and bot-p A-p) 7)
                              ((and bot-p Z-p) 9)
                              (top-p 2)
                              (bot-p 8)
                              (A-p 4)
                              (Z-p 6)
                              (t 5))))
                 (yang (gnugo-yang (char-after p))))
            (add-text-properties p (1+ p)
                                 `(gnugo-position
                                   ,position
                                   gnugo-yin
                                   ,yin
                                   gnugo-yang
                                   ,yang
                                   category
                                   ,(gnugo-yy yin yang)
                                   front-sticky
                                   (gnugo-position gnugo-yin))))
          (unless (= (1- other-edge) p)
            (add-text-properties (1+ p) (+ 2 p) ispc-props)
            (put-text-property p (+ 2 p) 'intangible ival)))
        (add-text-properties (1+ other-edge) right-empty grid-props)
        (goto-char right-empty)
        (when (looking-at "\\s-+\\(WH\\|BL\\).*capt.* \\([0-9]+\\).*$")
          (let ((prop (if (string= "WH" (match-string 1))
                          :white-captures
                        :black-captures))
                (beg (match-beginning 2))
                (end (match-end 2)))
            (put-text-property beg end :gnugo-cf (cons (- end beg) prop))
            (gnugo-put prop (match-string-no-properties 2))))
        (end-of-line)
        (put-text-property right-empty (point) 'category %rpad)
        (forward-char 1)))
    (add-text-properties (1- (point)) (point-max) grid-props)
    (skip-chars-forward " ")
    (put-text-property (1- (point)) (point) 'category %gpad)
    (put-text-property (point) (progn (end-of-line) (point))
                       'category %gspc)))

(defun gnugo-merge-showboard-results ()
  (let ((aft (substring (gnugo--q "showboard") 3))
        (adj 1)                         ; string to buffer position adjustment

        (sync "[0-9]* stones$")
        ;; Note: `sync' used to start w/ "[0-9]+", but that is too
        ;; restrictive a condition that fails in the case of:
        ;;
        ;; (before)
        ;;   ... WHITE has captured 1 stones
        ;;                           ^
        ;; (after)
        ;;   ... WHITE has captured 14 stones
        ;;                           ^
        ;;
        ;; where the after count has more digits than the before count,
        ;; but shares the same leading digits.  In this case, the result
        ;; of `compare-strings' points to the SPC following the before
        ;; count (indicated by caret in this example).

        (bef (buffer-substring-no-properties (point-min) (point-max)))
        (bef-start 0) (bef-idx 0)
        (aft-start 0) (aft-idx 0)
        aft-sync-backtrack mis inc cut new very-strange)
    (while (numberp (setq mis (gnugo--compare-strings
                               bef bef-start
                               aft aft-start)))
      (setq aft-sync-backtrack nil
            inc (if (cl-minusp mis)
                    (- (+ 1 mis))
                  (- mis 1))
            bef-idx (+ bef-start inc)
            aft-idx (+ aft-start inc)
            bef-start (if (eq bef-idx (string-match sync bef bef-idx))
                          (match-end 0)
                        (1+ bef-idx))
            aft-start (if (and (eq aft-idx (string-match sync aft aft-idx))
                               (let ((peek (1- aft-idx)))
                                 (while (not (= 32 (aref aft peek)))
                                   (setq peek (1- peek)))
                                 (setq aft-sync-backtrack (1+ peek))))
                          (match-end 0)
                        (1+ aft-idx))
            cut (+ bef-idx adj
                   (if aft-sync-backtrack
                       (- aft-sync-backtrack aft-idx)
                     0)))
      (goto-char cut)
      (if aft-sync-backtrack
          (let* ((asb aft-sync-backtrack)
                 (l-p (get-text-property cut :gnugo-cf))
                 (old-len (car l-p))
                 (capprop (cdr l-p))
                 (keep (text-properties-at cut)))
            (setq new (substring aft asb (string-match " " aft asb)))
            (plist-put keep :gnugo-cf (cons (length new) capprop))
            (gnugo-put capprop new)
            (delete-char old-len)
            (insert (apply 'propertize new keep))
            (incf adj (- (length new) old-len)))
        (setq new (aref aft aft-idx))
        (insert-and-inherit (char-to-string new))
        (let ((yin (get-text-property cut 'gnugo-yin))
              (yang (gnugo-yang new)))
          (add-text-properties cut (1+ cut)
                               `(gnugo-yang
                                 ,yang
                                 category
                                 ,(gnugo-yy yin yang))))
        (delete-char 1)
        ;; do this last to avoid complications w/ font lock
        ;; (this also means we cannot include `intangible' in `front-sticky')
        (when (setq very-strange (get-text-property (1+ cut) 'intangible))
          (put-text-property cut (1+ cut) 'intangible very-strange))))))

(defun gnugo-move-history (&optional rsel)
  "Determine and return the game's move history.
Optional arg RSEL controls side effects and return value.
If nil, display the history in the echo area as \"(N moves)\"
followed by the space-separated list of moves.  When called
interactively with a prefix arg (i.e., RSEL is `(4)'), display
similarly, but suffix with the mover (either \":B\" or \":W\").
If RSEL is the symbol `car' return the most-recent move; if
`cadr', the next-to-most-recent move; if `count' the number of
moves thus far.

For all other values of RSEL, do nothing and return nil."
  (interactive "P")
  (let ((size (gnugo-treeroot :SZ))
        col
        monkey mem
        acc node mprop move)
    (setq monkey (gnugo-get :monkey)
          mem (aref monkey 1))
    (cl-labels
        ((as-pos (cc) (if (string= "tt" cc)
                          "PASS"
                        (setq col (aref cc 0))
                        (format "%c%d"
                                (+ ?A (- (if (> ?i col) col (1+ col)) ?a))
                                (- size (- (aref cc 1) ?a)))))
         (next (byp) (when (setq node (caar mem)
                                 mprop (or (assq :B node)
                                           (assq :W node)))
                       (setq move (as-pos (cdr mprop))
                             mem (cdr mem))
                       (push (if byp
                                 (format "%s%s" move (car mprop))
                               move)
                             acc)))
         (tell () (message "(%d moves) %s"
                           (length acc)
                           (mapconcat 'identity (nreverse acc) " ")))
         (finish (byp) (while (next byp)) (tell)))
      (pcase rsel
        (`(4) (finish t))
        (`nil (finish nil))
        (`car              (car (next nil)))
        (`cadr  (next nil) (car (next nil)))
        (`count (aref monkey 2))
        (_ nil)))))

(defun gnugo-boss-is-near ()
  "Do `bury-buffer' until the current one is not a GNU Board."
  (interactive)
  (while (gnugo-board-buffer-p)
    (bury-buffer)))

(defun gnugo-note (property value &optional movep mogrifyp)
  (when mogrifyp
    (let ((sz (gnugo-treeroot :SZ)))
      (cl-labels
          ((mog (pos) (if (string= "PASS" pos)
                          "tt"
                        (let* ((col (aref pos 0))
                               (one (+ ?a (- col (if (< ?H col) 1 0) ?A)))
                               (two (+ ?a (- sz (string-to-number
                                                 (substring pos 1))))))
                          (format "%c%c" one two)))))
        (setq value (if (consp value)
                        (mapcar #'mog value)
                      (mog value))))))
  (let* ((fruit (list (cons property value)))
         (monkey (gnugo-get :monkey))
         (loc (aref monkey 0)))
    (if movep
        (let ((mem (aref monkey 1)))
          ;; todo: do variation check/merge/branch here.
          (setcdr loc (list fruit))
          (aset monkey 0 (setq loc (cdr loc)))
          (aset monkey 1 (cons loc mem))
          (aset monkey 2 (1+ (aref monkey 2))))
      (setcdr (last (car loc)) fruit))))

(defun gnugo-close-game (end-time resign)
  (gnugo-put :game-end-time end-time)
  (let ((now (or end-time (current-time))))
    (gnugo-put :scoring-seed (logior (ash (logand (car now) 255) 16)
                                     (cadr now))))
  (gnugo-put :game-over
    (if (or (eq t resign)
            (and (stringp resign)
                 (string-match "[BW][+][Rr]esign" resign)))
        (cl-labels
            ((ls (color) (mapcar
                          (lambda (x)
                            (cons (list color)
                                  (split-string x)))
                          (split-string
                           (gnugo-query "worm_stones %s" color)
                           "\n"))))
          (let ((live (append (ls "black") (ls "white"))))
            `((live ,@live)
              (dead))))
      (let ((dd (gnugo-query "dragon_data"))
            (start 0) mem color ent live dead)
        (while (string-match "\\(.+\\):\n[^ ]+[ ]+\\(black\\|white\\)\n"
                             dd start)
          (setq mem (match-string 1 dd)
                color (match-string 2 dd)
                start (match-end 0)
                ent (cons (list color)
                          (sort (gnugo-lsquery "dragon_stones %s" mem)
                                'string<)))
          (string-match "\nstatus[ ]+\\(\\(ALIVE\\)\\|[A-Z]+\\)\n"
                        dd start)
          (if (match-string 2 dd)
              (push ent live)
            (push ent dead))
          (setq start (match-end 0)))
        `((live ,@live)
          (dead ,@dead))))))

(defun gnugo--unclose-game ()
  (dolist (prop '(:game-over            ; all those in -close-game
                  :scoring-seed
                  :game-end-time))
    (gnugo-put prop nil))
  (let* ((root (car (gnugo-get :sgf-gametree)))
         (cur (assq :RE root)))
    (when cur
      (assert (not (eq cur (car root))) nil
              ":RE at head of root node: %S"
              root)
      (delq cur root))))

(defun gnugo-push-move (userp move)
  (let* ((color (gnugo-get (if userp :user-color :gnugo-color)))
         (start (gnugo-get :waiting-start))
         (now (current-time))
         (resignp (string= "resign" move))
         (passp (string= "PASS" move))
         (head (gnugo-move-history 'car))
         (onep (and head (string= "PASS" head)))
         (donep (or resignp (and onep passp))))
    (unless passp
      (gnugo-merge-showboard-results))
    (gnugo-put :last-mover color)
    (when userp
      (gnugo-put :last-user-bpos (and (not passp) (not resignp) move)))
    (gnugo-note (if (string= "black" color) :B :W) move t (not resignp))
    (when resignp
      (gnugo-note :EV "resignation"))
    (when start
      (gnugo-put :last-waiting (cadr (time-subtract now start))))
    (when donep
      (gnugo-close-game now resignp))
    (gnugo-put :waiting-start (and (not donep) now))
    donep))

(defun gnugo-venerate (yin yang)
  (let* ((fg-yy (gnugo-yy yin yang))
         (fg-disp (or (get fg-yy 'display)
                      (get fg-yy 'do-not-display)))
         (fg-data (plist-get (cdr fg-disp) :data))
         (bg-yy (gnugo-yy yin (gnugo-yang ?.)))
         (bg-disp (or (get bg-yy 'display)
                      (get bg-yy 'do-not-display)))
         (bg-data (plist-get (cdr bg-disp) :data))
         (bop (lambda (s)
                (let* ((start 0)
                       (ncolors
                        (when (string-match "\\([0-9]+\\)\\s-+[0-9]+\"," s)
                          (setq start (match-end 0))
                          (string-to-number (match-string 1 s)))))
                  (while (and (not (cl-minusp ncolors))
                              (string-match ",\n" s start))
                    (setq start (match-end 0)
                          ncolors (1- ncolors)))
                  (string-match "\"" s start)
                  (match-end 0))))
         (new (copy-sequence fg-data))
         (lx (length fg-data))
         (sx (funcall bop fg-data))
         (sb (funcall bop bg-data))
         (color-key (aref new sx)))     ; blech, heuristic
    (while (< sx lx)
      (when (and (not (= color-key (aref new sx)))
                 (cl-plusp (random 4)))
        (aset new sx (aref bg-data sb)))
      (incf sx)
      (incf sb))
    (create-image new 'xpm t :ascent 'center)))

(defun gnugo-refresh (&optional nocache)
  "Update GNUGO Board buffer display.
While a game is in progress, parenthesize the last-played stone (no parens
for pass).  If the buffer is currently displayed in the selected window,
recenter the board (presuming there is extra space in the window).  Update
the mode line.  Lastly, move point to the last position played by the user,
if that move was not a pass.

Prefix arg NOCACHE requests complete reconstruction of the display, which may
be slow.  (This should normally be unnecessary; specify it only if the display
seems corrupted.)  NOCACHE is silently ignored when GNU Go is thinking about
its move."
  (interactive "P")
  (when (and nocache (not (gnugo-get :waitingp)))
    (gnugo-propertize-board-buffer))
  (let* ((last-mover (gnugo-get :last-mover))
         (other (gnugo-other last-mover))
         (move (gnugo-move-history 'car))
         (game-over (gnugo-get :game-over))
         window last)
    ;; last move
    (when move
      (let ((l-ov (gnugo-get :lparen-ov))
            (r-ov (gnugo-get :rparen-ov)))
        (if (member move '("PASS" "resign"))
            (mapc 'delete-overlay (list l-ov r-ov))
          (gnugo-goto-pos move)
          (let* ((p (point))
                 (hspec (gnugo-get :highlight-last-move-spec))
                 (display-value (nth 0 hspec))
                 (l-offset (nth 1 hspec))
                 (l-new-pos (+ p l-offset))
                 (r-action (nth 2 hspec)))
            (overlay-put l-ov 'display
                         (if (functionp display-value)
                             (funcall display-value p)
                           display-value))
            (move-overlay l-ov l-new-pos (1+ l-new-pos))
            (if r-action
                (funcall r-action r-ov)
              (move-overlay r-ov (+ l-new-pos 2) (+ l-new-pos 3)))))))
    ;; buffer name
    (rename-buffer (concat (gnugo-get :diamond)
                           (if game-over
                               (format "%s(game over)"
                                       (if (string= move "resign")
                                           (concat move "ation ")
                                         ""))
                             (format "%s(%s to play)"
                                     (if move (concat move " ") "")
                                     other))))
    ;; pall of death
    (when game-over
      (let ((live (cdr (assq 'live game-over)))
            (dead (cdr (assq 'dead game-over)))
            p pall)
        (unless (eq game-over (get-text-property 1 'game-over))
          (dolist (group (append live dead))
            (dolist (pos (cdr group))
              (gnugo-goto-pos pos)
              (setq p (point))
              (put-text-property p (1+ p) 'group group)))
          (put-text-property 1 2 'game-over game-over))
        (dolist (group live)
          (when (setq pall (cdar group))
            (mapc 'delete-overlay pall)
            (setcdr (car group) nil)))
        (dolist (group dead)
          (unless (cdar group)
            (let (ov pall c (color (caar group)))
              (setq c (if (string= "black" color) "x" "o"))
              (dolist (pos (cdr group))
                (gnugo-goto-pos pos)
                (setq p (point) ov (make-overlay p (1+ p)))
                (overlay-put
                 ov 'display
                 (if (gnugo-get :display-using-images)
                     ;; respect the dead individually; it takes more time
                     ;; but that's not a problem (for them)
                     (gnugo-venerate (get-text-property p 'gnugo-yin)
                                     (gnugo-yang (aref (upcase c) 0)))
                   (propertize c 'face 'font-lock-warning-face)))
                (push ov pall))
              (setcdr (car group) pall))))))
    ;; window update
    (when (setq window (get-buffer-window (current-buffer)))
      (let* ((gridp (not (memq :nogrid buffer-invisibility-spec)))
             (size (gnugo-treeroot :SZ))
             (under10p (< size 10))
             (h (- (truncate (- (window-height window)
                                (* size (gnugo-get :hmul))
                                (if gridp 2 0))
                             2)
                   (if gridp 0 1)))
             (edges (window-edges window))
             (right-w-edge (nth 2 edges))
             (avail-width (- right-w-edge (nth 0 edges)))
             (wmul (gnugo-get :wmul))
             (imagesp (symbol-plist (gnugo-f 'ispc)))
             (w (/ (- avail-width
                      (* size wmul)
                      (if imagesp
                          0
                        (1- size))
                      2                 ; between board and grid
                      (if gridp
                          (if under10p 2 4)
                        0))
                   2.0)))
        (dolist (pair `((tpad . ,(if (and h (cl-plusp h))
                                     `(display ,(make-string h 10))
                                   '(invisible :nogrid)))
                        (gpad . (display
                                 (space :align-to
                                        ,(+ w
                                            2.0
                                            (cond (imagesp (+ (* 0.5 wmul)
                                                              (if under10p
                                                                  -0.5
                                                                0.5)))
                                                  (under10p 0)
                                                  (t 1))))))
                        (gspc . ,(when imagesp
                                   `(display
                                     (space-width
                                      ,(-
                                        ;; DWR: image width alone => OBOE!
                                        ;;- wmul
                                        ;; NB: ‘(* wmul cw)’ is the same
                                        ;; as ‘(car (image-size ... t))’.
                                        (let ((cw (frame-char-width)))
                                          (/ (+ 1.0 (* wmul cw))
                                             cw))
                                        1.0)))))
                        (lpad . ,(let ((d `(display (space :align-to ,w))))
                                   ;; We distinguish between these cases to
                                   ;; workaround a display bug whereby the
                                   ;; `before-string' is omitted entirely (not
                                   ;; rendered) when interacting w/ the text
                                   ;; mode last-move left-paren for moves in
                                   ;; column A.
                                   (if gridp
                                       `(before-string
                                         ,(apply 'propertize " " d))
                                     d)))
                        (rpad . (display
                                 (space :align-to ,(1- avail-width))))))
          (setplist (gnugo-f (car pair)) (cdr pair)))))
    ;; mode line update
    (let ((cur (gnugo-get :mode-line)))
      (unless (equal cur gnugo-mode-line)
        (setq cur gnugo-mode-line)
        (gnugo-put :mode-line cur)
        (gnugo-put :mode-line-form
          (cond ((stringp cur)
                 (setq cur (copy-sequence cur))
                 (let (acc cut c)
                   (while (setq cut (string-match "~[bwpmtu]" cur))
                     (aset cur cut ?%)
                     (setq cut (1+ cut) c (aref cur cut))
                     (aset cur cut ?s)
                     (push
                      `(,(intern (format "squig-%c" c))
                        ,(case c
                           (?b '(or (gnugo-get :black-captures) 0))
                           (?w '(or (gnugo-get :white-captures) 0))
                           (?p '(gnugo-other (gnugo-get :last-mover)))
                           (?t '(let ((ws (gnugo-get :waiting-start)))
                                  (if ws
                                      (cadr (time-since ws))
                                    "-")))
                           (?u '(or (gnugo-get :last-waiting) "-"))
                           (?m '(gnugo-move-history 'count))))
                      acc))
                   `(let ,(delete-dups (copy-sequence acc))
                      (format ,cur ,@(reverse (mapcar 'car acc))))))
                (t cur))))
      (let ((form (gnugo-get :mode-line-form)))
        (setq mode-line-process
              (and form
                   ;; this dynamicism is nice but excessive in its wantonness
                   ;;- `(" [" (:eval ,form) "]")
                   ;; this dynamicism is ok because the user triggers it
                   (list (format " [%s]" (eval form))))))
      (force-mode-line-update))
    ;; last user move
    (when (setq last (gnugo-get :last-user-bpos))
      (gnugo-goto-pos last))))

;;;---------------------------------------------------------------------------
;;; Game play actions

(defun gnugo-get-move-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let* ((so-far (gnugo-get :get-move-string))
           (full   (gnugo-put :get-move-string (concat so-far string))))
      (when (string-match "^= \\(.+\\)\n\n" full)
        (let ((pos-or-pass (match-string 1 full)))
          (gnugo-put :get-move-string nil)
          (gnugo-put :waitingp nil)
          (gnugo-push-move nil pos-or-pass)
          (let ((buf (current-buffer)))
            (let (gnugo-inhibit-refresh)
              (run-hooks 'gnugo-post-move-hook)
              (unless gnugo-inhibit-refresh
                (with-current-buffer buf
                  (gnugo-refresh))))))))))

(defun gnugo-get-move (color)
  (gnugo-put :waitingp t)
  (gnugo--begin-exchange
      (gnugo-get :proc) 'gnugo-get-move-insertion-filter
    (concat "genmove " color))
  (accept-process-output))

(defun gnugo-cleanup ()
  (when (gnugo-board-buffer-p)
    (unless (zerop (buffer-size))
      (message "Thank you for playing GNU Go."))
    (mapc (lambda (sym)
            (setplist sym nil)          ; "...is next to fordliness." --Huxley
            ;; Sigh, "2nd arg optional" obsolete as of Emacs 23.3.
            ;; No worries, things will be Much Better w/ structs, RSN...
            (unintern sym nil))
          (append (gnugo-get :all-yy)
                  (mapcar 'gnugo-f
                          '(anim
                            tpad
                            gpad
                            gspc
                            lpad
                            rpad
                            ispc))))
    (setq gnugo-state nil)))

(defun gnugo-position ()
  (or (get-text-property (point) 'gnugo-position)
      (user-error "Not a proper position point")))

(defun gnugo-move ()
  "Make a move on the GNUGO Board buffer.
The position is computed from current point.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate t)
  (let* ((buf (current-buffer))
         (pos (gnugo-position))
         (move (format "play %s %s" (gnugo-get :user-color) pos))
         (accept (gnugo--q move)))
    (unless (= ?= (aref accept 0))
      (user-error "%s" accept))
    (gnugo-push-move t pos)             ; value always nil for non-pass move
    (let (gnugo-inhibit-refresh)
      (run-hooks 'gnugo-post-move-hook)
      (unless gnugo-inhibit-refresh
        (with-current-buffer buf
          (gnugo-refresh))))
    (with-current-buffer buf
      (gnugo-get-move (gnugo-get :gnugo-color)))))

(defun gnugo-mouse-move (e)
  "Do `gnugo-move' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (when (looking-at "[.+]")
    (gnugo-move)))

(defun gnugo-pass ()
  "Make a pass on the GNUGO Board buffer.
Signal error if done out-of-turn or if game-over.
To start a game try M-x gnugo."
  (interactive)
  (gnugo-gate t)
  (let ((accept (gnugo--q "play %s PASS" (gnugo-get :user-color))))
    (unless (= ?= (aref accept 0))
      (user-error "%s" accept)))
  (let ((donep (gnugo-push-move t "PASS"))
        (buf (current-buffer)))
    (let (gnugo-inhibit-refresh)
      (run-hooks 'gnugo-post-move-hook)
      (unless gnugo-inhibit-refresh
        (with-current-buffer buf
          (gnugo-refresh))))
    (unless donep
      (with-current-buffer buf
        (gnugo-get-move (gnugo-get :gnugo-color))))))

(defun gnugo-mouse-pass (e)
  "Do `gnugo-pass' at mouse location."
  (interactive "@e")
  (mouse-set-point e)
  (gnugo-pass))

(defun gnugo-resign ()
  (interactive)
  (gnugo-gate t)
  (if (not (y-or-n-p "Resign? "))
      (message "(not resigning)")
    (gnugo-push-move t "resign")
    (gnugo-refresh)))

(defun gnugo-animate-group (w/d)
  ;; W/D is a symbol, either ‘worm’ or ‘dragon’.
  (let* ((pos (gnugo-position))
         (orig-b-m-p (buffer-modified-p))
         blurb stones)
    (unless (memq (char-after) '(?X ?O))
      (user-error "No stone at %s" pos))
    (setq blurb (message "Computing %s stones ..." w/d)
          stones (gnugo-lsquery "%s_stones %s" w/d pos))
    (message "%s %s in group." blurb (length stones))
    (setplist (gnugo-f 'anim) nil)
    (let* ((spec (if (gnugo-get :display-using-images)
                     (loop with yin  = (get-text-property (point) 'gnugo-yin)
                           with yang = (gnugo-yang (char-after))
                           with up   = (get (gnugo-yy yin yang t) 'display)
                           with dn   = (get (gnugo-yy yin yang) 'display)
                           for n below (length gnugo-animation-string)
                           collect (if (zerop (logand 1 n))
                                       dn up))
                   (split-string gnugo-animation-string "" t)))
           (cell (list spec))
           (ovs (save-excursion
                  (mapcar (lambda (pos)
                            (gnugo-goto-pos pos)
                            (let* ((p (point))
                                   (ov (make-overlay p (1+ p))))
                              (overlay-put ov 'category (gnugo-f 'anim))
                              (overlay-put ov 'priority most-positive-fixnum)
                              ov))
                          stones))))
      (setplist (gnugo-f 'anim) (cons 'display cell))
      (while (and (cdr spec)            ; let last linger lest levity lost
                  (sit-for 0.08675309)) ; jenny jenny i got your number...
        (setcar cell (setq spec (cdr spec)))
        ;; Force redisplay of overlays.
        (set-buffer-modified-p orig-b-m-p))
      (sit-for 5)
      (mapc 'delete-overlay ovs)
      t)))

(defun gnugo-display-group-data (command buffer-name)
  (message "Computing %s ..." command)
  (let ((data (gnugo--q "%s %s" command (gnugo-position))))
    (switch-to-buffer buffer-name)
    (erase-buffer)
    (insert data))
  (message "Computing %s ... done." command))

(defun gnugo-worm-stones ()
  "In the GNUGO Board buffer, animate \"worm\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group 'worm))

(defun gnugo-worm-data ()
  "Display in another buffer data from \"worm\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "worm_data" "*gnugo worm data*"))

(defun gnugo-dragon-stones ()
  "In the GNUGO Board buffer, animate \"dragon\" at current position.
Signal error if done out-of-turn or if game-over.
See variable `gnugo-animation-string' for customization."
  (interactive)
  (gnugo-gate)
  (gnugo-animate-group 'dragon))

(defun gnugo-dragon-data ()
  "Display in another buffer data from \"dragon\" at current position.
Signal error if done out-of-turn or if game-over."
  (interactive)
  (gnugo-gate)
  (gnugo-display-group-data "dragon_data" "*gnugo dragon data*"))

(defun gnugo-toggle-dead-group ()
  "In a GNUGO Board buffer, during game-over, toggle a group as dead.
The group is selected from current position (point).  Signal error if
not in game-over or if there is no group at that position.

In the context of GNU Go, a group is called a \"dragon\" and may be
composed of more than one \"worm\" (set of directly-connected stones).
It is unclear to the gnugo.el author whether or not GNU Go supports
 - considering worms as groups in their own right; and
 - toggling group aliveness via GTP.
Due to these uncertainties, this command is only half complete; the
changes you may see in Emacs are not propagated to the gnugo subprocess.
Thus, GTP commands like `final_score' may give unexpected results.

If you are able to expose via GTP `change_dragon_status' in utils.c,
you may consider modifying the `gnugo-toggle-dead-group' source code
to enable full functionality."
  (interactive)
  (let ((game-over (or (gnugo-get :game-over)
                       (user-error "Sorry, game still in play")))
        (group (or (get-text-property (point) 'group)
                   (user-error "No stone at that position")))
        (now (current-time)))
    (gnugo-put :scoring-seed (logior (ash (logand (car now) 255) 16)
                                     (cadr now)))
    (let ((live (assq 'live game-over))
          (dead (assq 'dead game-over))
          bef now)
      (if (memq group live)
          (setq bef live now dead)
        (setq bef dead now live))
      (setcdr bef (delq group (cdr bef)))
      (setcdr now (cons group (cdr now)))
      ;; disabled permanently -- too wrong
      (when nil
        (cl-labels
            ((populate (group)
                       (let ((color (caar group)))
                         (dolist (stone (cdr group))
                           (gnugo-query "play %s %s" color stone)))))
          (if (eq now live)
              (populate group)
            ;; drastic (and wrong -- clobbers capture info, etc)
            (gnugo-query "clear_board")
            (mapc #'populate (cdr live)))))
      ;; here is the desired interface (to be enabled Some Day)
      (when nil
        (gnugo-query "change_dragon_status %s %s"
                     (cadr group) (if (eq now live)
                                      'alive
                                    'dead)))))
  (save-excursion
    (gnugo-refresh)))

(defun gnugo-estimate-score ()
  "Display estimated score of a game of GNU Go.
Output includes number of stones on the board and number of stones
captured by each player, and the estimate of who has the advantage (and
by how many stones)."
  (interactive)
  (message "Est.score ...")
  (let ((black (length (gnugo-lsquery "list_stones black")))
        (white (length (gnugo-lsquery "list_stones white")))
        (black-captures (gnugo-query "captures black"))
        (white-captures (gnugo-query "captures white"))
        (est (gnugo-query "estimate_score")))
    ;; might as well update this
    (gnugo-put :black-captures black-captures)
    (gnugo-put :white-captures white-captures)
    (message "Est.score ... B %s %s | W %s %s | %s"
             black black-captures white white-captures est)))

(defun gnugo-write-sgf-file (filename)
  "Save the game history to FILENAME (even if unfinished).
If FILENAME already exists, Emacs confirms that you wish to overwrite it."
  (interactive "FWrite game as SGF file: ")
  (when (and (file-exists-p filename)
             (not (y-or-n-p "File exists. Continue? ")))
    (user-error "Not writing %s" filename))
  (gnugo/sgf-write-file (gnugo-get :sgf-collection) filename)
  (set-buffer-modified-p nil))

(defun gnugo--who-is-who (wait play samep)
  (message "GNU Go %splays as %s, you as %s (%s)"
           (if samep "" "now ")
           wait play (if samep
                         "as before"
                       "NOTE: this is a switch!")))

(defun gnugo-read-sgf-file (filename)
  "Load the first game tree from FILENAME, a file in SGF format."
  (interactive "fSGF file to load: ")
  (when (file-directory-p filename)
    (user-error "Cannot load a directory (try a filename with extension .sgf)"))
  (let (ans play wait samep coll tree)
    ;; problem: requiring GTP `loadsgf' complicates network subproc support;
    ;; todo: skip it altogether when confident about `gnugo/sgf-read-file'
    (unless (= ?= (aref (setq ans (gnugo--q "loadsgf %s"
                                            (expand-file-name filename)))
                        0))
      (user-error "%s" ans))
    (setq play (substring ans 2)
          wait (gnugo-other play)
          samep (string= (gnugo-get :user-color) play))
    (gnugo-put :last-mover wait)
    (unless samep
      (gnugo-put :gnugo-color wait)
      (gnugo-put :user-color play))
    (setq coll (gnugo/sgf-read-file filename)
          tree (nth (let ((n (length coll)))
                      ;; This is better:
                      ;; (if (= 1 n)
                      ;;     0
                      ;;   (let* ((q (format "Which game? (1-%d)" n))
                      ;;          (choice (1- (read-number q 1))))
                      ;;     (if (and (< -1 choice) (< choice n))
                      ;;         choice
                      ;;       (message "(Selecting the first game)")
                      ;;       0)))
                      ;; but this is what we use (for now) to accomodate
                      ;; (aka faithfully mimic) GTP `loadsgf' limitations:
                      (unless (= 1 n)
                        (message "(Selecting the first game)"))
                      0)
                    coll))
    (gnugo-put :sgf-collection coll)
    (gnugo-put :sgf-gametree tree)
    (let* ((loc tree)
           (count 0)
           mem node play game-over)
      (while (setq node (car loc))
        (when (setq play (or (assq :B node)
                             (assq :W node)))
          ;; SGF[4] allows "" to mean PASS.  For now,
          ;; we normalize here instead of at the lower layer.
          (when (string= "" (cdr play))
            (setcdr play "tt"))
          (incf count)
          (push loc mem))
        (setq loc (cdr loc)))
      (gnugo-put :game-over
        (setq game-over
              (or (cdr (assq :RE (car tree)))
                  (and (cdr mem)
                       (equal '("tt" "tt")
                              (let ((order (if (string= "black" wait)
                                               '(:B :W)
                                             '(:W :B))))
                                (mapcar (lambda (pk)
                                          (cdr (assq (funcall pk order)
                                                     (car (funcall pk mem)))))
                                        '(car cadr))))
                       'two-passes))))
      (gnugo-put :monkey
        (vector (or (car mem) tree)
                mem
                count))
      (when (and game-over
                 ;; (maybe) todo: user var to inhibit (can be slow)
                 t)
        (gnugo-close-game nil game-over)))
    (gnugo-refresh t)
    (set-buffer-modified-p nil)
    (gnugo--who-is-who wait play samep)))

(defun gnugo-magic-undo (spec &optional noalt)
  "Undo moves on the GNUGO Board, based on SPEC, a string or number.
If SPEC is a string in the form of a board position (e.g., \"T19\"),
check that the position is occupied by a stone of the user's color,
and if so, remove moves from the history until that position is clear.
If SPEC is a positive number, remove exactly that many moves from the
history, signaling an error if the history is exhausted before finishing.
If SPEC is not recognized, signal \"bad spec\" error.

Refresh the board for each move undone.  If (in the case where SPEC is
a number) after finishing, the color to play is not the user's color,
schedule a move by GNU Go.

After undoing the move(s), schedule a move by GNU Go if it is GNU Go's
turn to play.  Optional second arg NOALT non-nil inhibits this."
  (gnugo-gate)
  (let* ((n 0)
         (user-color (gnugo-get :user-color))
         (monkey (gnugo-get :monkey))
         (mem (aref monkey 1))
         (count (aref monkey 2))
         done ans)
    (cond ((and (numberp spec) (cl-plusp spec))
           (setq n spec done (lambda () (zerop n))))
          ((string-match "^[a-z]" spec)
           (let ((pos (upcase spec)))
             (setq done `(lambda ()
                           (gnugo-goto-pos ,pos)
                           (memq (char-after) '(?. ?+))))
             (when (funcall done)
               (user-error "%s already clear" pos))
             (when (= (save-excursion
                        (gnugo-goto-pos pos)
                        (char-after))
                      (if (string= "black" user-color)
                          ?O
                        ?X))
               (user-error "%s not occupied by %s" pos user-color))))
          (t (user-error "Bad spec: %S" spec)))
    (when (gnugo-get :game-over)
      (gnugo--unclose-game))
    (while (not (funcall done))
      (setq ans (gnugo--q "undo"))
      (unless (= ?= (aref ans 0))
        (user-error "%s" ans))
      (aset monkey 2 (decf count))
      (aset monkey 1 (setq mem (cdr mem)))
      (aset monkey 0 (or (car mem) (gnugo-get :sgf-gametree)))
      (gnugo-put :last-mover (gnugo-other (gnugo-get :last-mover)))
      (gnugo-merge-showboard-results)   ; all
      (gnugo-refresh)                   ; this
      (decf n)                          ; is
      (redisplay))                      ; eye candy
    (let* ((ulastp (string= (gnugo-get :last-mover) user-color))

           (ubpos (gnugo-move-history (if ulastp 'car 'cadr))))
      (gnugo-put :last-user-bpos (if (and ubpos (not (string= "PASS" ubpos)))
                                     ubpos
                                   (gnugo-get :center-position)))
      (gnugo-refresh t)
      ;; preserve restricted-functionality semantics (todo: remove restriction)
      (setcdr (aref monkey 0) nil)
      (when (and ulastp (not noalt))
        (gnugo-get-move (gnugo-get :gnugo-color))))))

(defun gnugo-undo-one-move (&optional me-next)
  "Undo exactly one move (perhaps GNU Go's, perhaps yours).
Do not schedule a move by GNU Go even if it is GNU Go's turn to play.
Prefix arg ME-NEXT means to arrange for you to play
the color of the next move (and GNU Go the opposite).
This is useful after loading an SGF file whose last
move was done by the color you prefer to play:
 \\[gnugo-read-sgf-file] FILENAME RET
 C-u \\[gnugo-undo-one-move]

See also `gnugo-undo-two-moves'."
  (interactive "P")
  (gnugo-gate)
  (gnugo-magic-undo 1 t)
  (when me-next
    (let* ((wait (gnugo-get :last-mover))
           (play (gnugo-other wait)))
      (gnugo--who-is-who wait play (string= play (gnugo-get :user-color)))
      (gnugo-put :user-color play)
      (gnugo-put :gnugo-color wait))))

(defun gnugo-undo-two-moves ()
  "Undo a pair of moves (GNU Go's and yours).
However, if you are the last mover, undo only one move.
Regardless, after undoing, it is your turn to play again."
  (interactive)
  (gnugo-gate)
  (gnugo-magic-undo (if (string= (gnugo-get :user-color)
                                 (gnugo-get :last-mover))
                        1
                      2)))

(defun gnugo-display-final-score ()
  "Display final score and other info in another buffer (when game over).
If the game is still ongoing, Emacs asks if you wish to stop play (by
making sure two \"pass\" moves are played consecutively, if necessary).
Also, add the `:RE' SGF property to the root node of the game tree."
  (interactive)
  (let ((game-over (gnugo-get :game-over)))
    (unless (or game-over
                (and (not (gnugo-get :waitingp))
                     (y-or-n-p "Game still in play. Stop play now? ")))
      (user-error "Sorry, game still in play"))
    (unless game-over
      (cl-labels
          ((pass (userp)
                 (message "Playing PASS for %s ..."
                          (gnugo-get (if userp :user-color :gnugo-color)))
                 (sit-for 1)
                 (gnugo-push-move userp "PASS")))
        (unless (pass t)
          (pass nil)))
      (gnugo-refresh)
      (sit-for 3)))
  (let ((b=  "   Black = ")
        (w=  "   White = ")
        (res (let* ((node (car (aref (gnugo-get :monkey) 0)))
                    (event (and node (cdr (assq :EV node)))))
               (and event (string= "resignation" event)
                    (if (assq :B node) "black" "white"))))
        blurb result)
    (if res
        (setq blurb (list
                     (format "%s wins.\n"
                             (substring (if (= ?b (aref res 0)) w= b=)
                                        3 8))
                     "The game is over.\n"
                     (format "Resignation by %s.\n" res))
              result (concat (upcase (substring (gnugo-other res) 0 1))
                             "+Resign"))
      (message "Computing final score ...")
      (let* ((g-over (gnugo-get :game-over))
             (live   (cdr (assq 'live g-over)))
             (dead   (cdr (assq 'dead g-over)))
             (seed   (gnugo-get :scoring-seed))
             (terr-q (format "final_status_list %%s_territory %d" seed))
             (terr   "territory")
             (capt   "captures")
             (b-terr (length (gnugo-lsquery terr-q "black")))
             (w-terr (length (gnugo-lsquery terr-q "white")))
             (b-capt (string-to-number (gnugo-get :black-captures)))
             (w-capt (string-to-number (gnugo-get :white-captures)))
             (komi   (gnugo-treeroot :KM)))
        (setq blurb (list "The game is over.  Final score:\n")
              result (gnugo-query "final_score %d" seed))
        (cond ((string= "Chinese" (gnugo-treeroot :RU))
               (dolist (group live)
                 (incf (if (string= "black" (caar group))
                           b-terr
                         w-terr)
                       (length (cdr group))))
               (dolist (group dead)
                 (incf (if (string= "black" (caar group))
                           w-terr
                         b-terr)
                       (length (cdr group))))
               (push (format "%s%d %s = %3.1f\n" b= b-terr terr b-terr) blurb)
               (push (format "%s%d %s + %3.1f %s = %3.1f\n" w=
                             w-terr terr komi 'komi (+ w-terr komi))
                     blurb))
              (t
               (dolist (group dead)
                 (incf (if (string= "black" (caar group))
                           w-terr
                         b-terr)
                       (* 2 (length (cdr group)))))
               (push (format "%s%d %s + %s %s = %3.1f\n" b=
                             b-terr terr
                             b-capt capt
                             (+ b-terr b-capt))
                     blurb)
               (push (format "%s%d %s + %s %s + %3.1f %s = %3.1f\n" w=
                             w-terr terr
                             w-capt capt
                             komi 'komi
                             (+ w-terr w-capt komi))
                     blurb)))
        (push (if (string= "0" result)
                  "The game is a draw.\n"
                (format "%s wins by %s.\n"
                        (substring (if (= ?B (aref result 0)) b= w=) 3 8)
                        (substring result 2)))
              blurb)
        (message "Computing final score ... done")))
    ;; extra info
    (let ((beg (gnugo-get :game-start-time))
          (end (gnugo-get :game-end-time)))
      (when end
        (push "\n" blurb)
        (cl-labels
            ((yep (pretty moment)
                  (push (format-time-string
                         (concat pretty ": %Y-%m-%d %H:%M:%S %z\n")
                         moment)
                        blurb)))
          (yep "Game start" beg)
          (yep "       end" end))))
    (setq blurb (apply 'concat (nreverse blurb)))
    (let* ((root (car (gnugo-get :sgf-gametree)))
           (cur (assq :RE root)))
      (if cur
          (setcdr cur result)
        (setcdr (last root) (list (cons :RE result)))))
    (switch-to-buffer (format "%s*GNUGO Final Score*" (gnugo-get :diamond)))
    (erase-buffer)
    (insert blurb)))

(defun gnugo-quit ()
  "Kill the current buffer, assumed to be in GNUGO Board mode, maybe.
If the game is not over, ask for confirmation first."
  (interactive)
  (if (or (gnugo-get :game-over)
          (y-or-n-p "Quit? "))
      (kill-buffer nil)
    (message "(not quitting)")))

(defun gnugo-leave-me-alone ()
  "Kill the current buffer unconditionally."
  (interactive)
  (kill-buffer nil))

(defun gnugo-fancy-undo (count)
  "Rewind the game tree in various ways.
Prefix arg COUNT means to undo that many moves.
Otherwise, undo repeatedly up to and including the move
which placed the stone at point."
  (interactive "P")
  (gnugo-magic-undo
   ;; TODO: Move this into `gnugo-magic-undo' proper.
   (cond ((numberp count) count)
         ((consp count) (car count))
         (t (gnugo-position)))))

(defun gnugo-toggle-image-display-command () ; ugh
  "Toggle use of images to display the board, then refresh."
  (interactive)
  (gnugo-toggle-image-display)
  (save-excursion (gnugo-refresh)))

(defun gnugo-describe-position ()
  "Display the board position under cursor in the echo area."
  (interactive)
  (message "%s" (gnugo-position)))

(defun gnugo-switch-to-another ()
  "Switch to another GNU Go game buffer (if any)."
  (interactive)
  (loop for buf in (cdr (buffer-list))
        if (gnugo-board-buffer-p buf)
        return (progn
                 (bury-buffer)
                 (switch-to-buffer buf))
        finally do (message "(only one)")))

;;;---------------------------------------------------------------------------
;;; Command properties and gnugo-command

;; GTP commands entered by the user are never issued directly to GNU Go;
;; instead, their behavior and output are controlled by the property
;; `:gnugo-gtp-command-spec' hung off of each (interned/symbolic) command.
;; The value of this property is a sub-plist, w/ sub-properties as follows:
;;
;; :full -- completely interpret the command string; the value is a
;;          func that takes the list of words derived from splitting the
;;          command string (minus the command) and handles everything.
;;
;; :output -- either a keyword specifying the preferred output method:
;;              :message -- show output in minibuffer
;;              :discard -- sometimes you just don't care;
;;            or a function that takes one arg, the output string, and
;;            handles it completely.   default is to switch to buffer
;;            "*gnugo command output*" if the output has a newline,
;;            otherwise use `message'.
;;
;; :post-thunk -- run after output processing (at the very end).

(defun gnugo-command (command)
  "Send the Go Text Protocol COMMAND (a string) to GNU Go.
Output and Emacs behavior depend on which command is given (some
commands are handled completely by Emacs w/o using the subprocess;
some commands have their output displayed in specially prepared
buffers or in the echo area; some commands are instrumented to do
gnugo.el-specific housekeeping).

For example, for the command \"help\", Emacs visits the
GTP command reference info page.

NOTE: At this time, GTP command handling specification is still
      incomplete.  Thus, some commands WILL confuse gnugo.el."
  (interactive "sCommand: ")
  (if (string= "" command)
      (message "(no command given)")
    (let* ((split (split-string command))
           (cmd (intern (car split)))
           (spec (get cmd :gnugo-gtp-command-spec))
           (full (plist-get spec :full)))
      (if full
          (funcall full (cdr split))
        (message "Doing %s ..." command)
        (let* ((ans (gnugo--q command))
               (where (plist-get spec :output)))
          (if (string-match "unknown.command" ans)
              (message "%s" ans)
            (cond ((functionp where) (funcall where ans))
                  ((eq :discard where) (message ""))
                  ((or (eq :message where)
                       (not (string-match "\n" ans)))
                   (message "%s" ans))
                  (t (switch-to-buffer "*gnugo command output*")
                     (erase-buffer)
                     (insert ans)
                     (message "Doing %s ... done." command)))
            (let ((thunk (plist-get spec :post-thunk)))
              (when thunk (funcall thunk)))))))))

;;;---------------------------------------------------------------------------
;;; Major mode for interacting with a GNUGO subprocess

(put 'gnugo-board-mode 'mode-class 'special)
(defun gnugo-board-mode ()
  "Major mode for playing GNU Go.
Entering this mode runs the normal hook `gnugo-board-mode-hook'.
In this mode, keys do not self insert.

\\{gnugo-board-mode-map}"
  (switch-to-buffer (generate-new-buffer "(Uninitialized GNUGO Board)"))
  (buffer-disable-undo)                 ; todo: undo undo undoing
  (kill-all-local-variables)
  (setq truncate-lines t)
  (use-local-map gnugo-board-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(gnugo-font-lock-keywords t))
  (setq major-mode 'gnugo-board-mode)
  (setq mode-name "GNUGO Board")
  (add-hook 'kill-buffer-hook 'gnugo-cleanup nil t)
  (set (make-local-variable 'gnugo-state)
       (make-hash-table :size (1- 42) :test 'eq))
  (add-to-invisibility-spec :nogrid)
  (mapc (lambda (prop)
          (gnugo-put prop nil))         ; todo: separate display/game aspects;
        '(:game-over                    ;       move latter to func `gnugo'
          :waitingp
          :last-waiting
          :black-captures
          :white-captures
          :mode-line
          :mode-line-form
          :display-using-images
          :xpms
          :local-xpms
          :all-yy))
  (let ((name (if (string-match "[ ]" gnugo-program)
                  (let ((p (substring gnugo-program 0 (match-beginning 0)))
                        (o (substring gnugo-program (match-end 0)))
                        (h (or (car gnugo-option-history) "")))
                    (when (string-match "--mode" o)
                      (user-error "Found \"--mode\" in `gnugo-program'"))
                    (when (and o (cl-plusp (length o))
                               h (cl-plusp (length o))
                               (or (< (length h) (length o))
                                   (not (string= (substring h 0 (length o))
                                                 o))))
                      (push (concat o " " h) gnugo-option-history))
                    p)
                gnugo-program))
        (args (read-string "GNU Go options: "
                           (car gnugo-option-history)
                           'gnugo-option-history))
        (rules "Japanese")
        proc
        board-size user-color handicap komi minus-l infile)
    (dolist (x '((board-size      19 "--boardsize")
                 (user-color "black" "--color" "\\(black\\|white\\)")
                 (handicap         0 "--handicap")
                 (komi           0.0 "--komi")
                 (minus-l        nil "\\([^-]\\|^\\)-l[ ]*" "[^ ]+")
                 (infile         nil "--infile" "[ ]*[^ ]+")))
      (destructuring-bind (var default opt &optional rx) x
        (set var
             (or (when (string-match opt args)
                   (let ((start (match-end 0)) s)
                     (string-match (or rx "[0-9.]+") args start)
                     (setq s (match-string 0 args))
                     (if rx s (string-to-number s))))
                 default))))
    (gnugo-put :user-color user-color)
    (when (string-match "--chinese-rules" args)
      (setq rules "Chinese"))
    (let ((proc-args (split-string args)))
      (gnugo-put :proc-args proc-args)
      (gnugo-put :proc (setq proc (apply 'start-process "gnugo"
                                         (current-buffer) name
                                         "--mode" "gtp" "--quiet"
                                         proc-args))))
    (set-process-sentinel proc 'gnugo-sentinel)
    ;; Emacs is too protective sometimes, blech.
    (set-process-query-on-exit-flag proc nil)
    (when (or minus-l infile)
      (dolist (x '((board-size "query_boardsize")
                   (komi       "get_komi")
                   (handicap   "get_handicap")))
        (destructuring-bind (prop q) x
          (set prop (string-to-number (gnugo-query q))))))
    (gnugo-put :diamond (substring (process-name proc) 5))
    (gnugo-put :gnugo-color (gnugo-other user-color))
    (gnugo-put :highlight-last-move-spec
      (gnugo-put :default-highlight-last-move-spec '("(" -1 nil)))
    (gnugo-put :lparen-ov (make-overlay 1 1))
    (gnugo-put :rparen-ov (let ((ov (make-overlay 1 1)))
                            (overlay-put ov 'display ")")
                            ov))
    (let ((tree (list (list '(:FF . 4) '(:GM . 1)))))
      (gnugo-put :sgf-gametree tree)
      (gnugo-put :sgf-collection (list tree))
      (gnugo-put :monkey (vector tree nil 0)))
    (let ((g-blackp (string= "white" user-color)))
      (mapc (lambda (x) (apply 'gnugo-note x))
            `((:SZ ,board-size)
              (:DT ,(format-time-string "%Y-%m-%d"))
              (:RU ,rules)
              (:AP ("gnugo.el" . ,gnugo-version))
              (:KM ,komi)
              (,(if g-blackp :PW :PB) ,(user-full-name))
              (,(if g-blackp :PB :PW) ,(concat "GNU Go "
                                               (gnugo-query "version")))
              ,@(when (not (zerop handicap))
                  `((:HA ,handicap)
                    (:AB ,(gnugo-lsquery "fixed_handicap %d" handicap)
                         nil t)))))))
  (gnugo-put :waiting-start (current-time))
  (gnugo-put :hmul 1)
  (gnugo-put :wmul 1)
  (run-hooks 'gnugo-board-mode-hook)
  (gnugo-refresh t))

;;;---------------------------------------------------------------------------
;;; Entry point

;;;###autoload
(defun gnugo (&optional new-game)
  "Run gnugo in a buffer, or resume a game in progress.
Prefix arg means skip the game-in-progress check and start a new
game straight away.
\\<gnugo-board-mode-map>
To play, use \\[gnugo-move] to place a stone or \\[gnugo-pass] to pass.

You are queried for additional command-line options (Emacs supplies
\"--mode gtp --quiet\" automatically).  Here is a list of options
that gnugo.el understands and handles specially:

    --boardsize num   Set the board size to use (5--19)
    --color <color>   Choose your color ('black' or 'white')
    --handicap <num>  Set the number of handicap stones (0--9)

If there is already a game in progress you may resume it instead of
starting a new one.  See `gnugo-board-mode' documentation for more info."
  (interactive "P")
  (let* ((all (let (acc)
                (dolist (buf (buffer-list))
                  (when (gnugo-board-buffer-p buf)
                    (push (cons (buffer-name buf) buf) acc)))
                acc))
         (n (length all)))
    (if (and (not new-game)
             (cl-plusp n)
             (y-or-n-p (format "GNU Go game%s in progress, resume play? "
                               (if (= 1 n) "" "s"))))
        ;; resume
        (switch-to-buffer
         (cdr (if (= 1 n)
                  (car all)
                (let ((sel (completing-read "Which one? " all nil t)))
                  (if (string= "" sel)
                      (car all)
                    (assoc sel all))))))
      ;; set up a new board
      (gnugo-board-mode)
      (let ((half (truncate (1+ (gnugo-treeroot :SZ)) 2)))
        (gnugo-goto-pos (format "A%d" half))
        (forward-char (* 2 (1- half)))
        (gnugo-put :last-user-bpos
          (gnugo-put :center-position
            (get-text-property (point) 'gnugo-position))))
      ;; first move
      (gnugo-put :game-start-time (current-time))
      (let ((g (gnugo-get :gnugo-color))
            (n (or (gnugo-treeroot :HA) 0))
            (u (gnugo-get :user-color)))
        (gnugo-put :last-mover g)
        (when (or (and (string= "black" u) (< 1 n))
                  (and (string= "black" g) (< n 2)))
          (gnugo-put :last-mover u)
          (gnugo-refresh t)
          (gnugo-get-move g))))))

;;;---------------------------------------------------------------------------
;;; Load-time actions

(unless gnugo-board-mode-map
  (setq gnugo-board-mode-map (make-sparse-keymap))
  (suppress-keymap gnugo-board-mode-map)
  (mapc (lambda (pair)
          (define-key gnugo-board-mode-map (car pair) (cdr pair)))
        '(("?"        . describe-mode)
          ("\C-m"     . gnugo-move)
          (" "        . gnugo-move)
          ("P"        . gnugo-pass)
          ("R"        . gnugo-resign)
          ("q"        . gnugo-quit)
          ("Q"        . gnugo-leave-me-alone)
          ("U"        . gnugo-fancy-undo)
          ("\M-u"     . gnugo-undo-one-move)
          ("u"        . gnugo-undo-two-moves)
          ("\C-?"     . gnugo-undo-two-moves)
          ("\C-l"     . gnugo-refresh)
          ("\M-_"     . gnugo-boss-is-near)
          ("_"        . gnugo-boss-is-near)
          ("h"        . gnugo-move-history)
          ("i"        . gnugo-toggle-image-display-command)
          ("w"        . gnugo-worm-stones)
          ("W"        . gnugo-worm-data)
          ("d"        . gnugo-dragon-stones)
          ("D"        . gnugo-dragon-data)
          ("t"        . gnugo-toggle-dead-group)
          ("g"        . gnugo-toggle-grid)
          ("!"        . gnugo-estimate-score)
          (":"        . gnugo-command)
          (";"        . gnugo-command)
          ("="        . gnugo-describe-position)
          ("s"        . gnugo-write-sgf-file)
          ("\C-x\C-s" . gnugo-write-sgf-file)
          ("\C-x\C-w" . gnugo-write-sgf-file)
          ("l"        . gnugo-read-sgf-file)
          ("F"        . gnugo-display-final-score)
          ("A"        . gnugo-switch-to-another)
          ;; mouse
          ([(down-mouse-1)] . gnugo-mouse-move)
          ([(down-mouse-2)] . gnugo-mouse-move) ; mitigate accidents
          ([(down-mouse-3)] . gnugo-mouse-pass)
          ;; delving into the curiosities
          ("\C-c\C-p" . gnugo-describe-internal-properties))))

(unless (get 'help :gnugo-gtp-command-spec)
  (cl-labels
      ((sget (x) (get x :gnugo-gtp-command-spec))
       (jam (cmd prop val) (put cmd :gnugo-gtp-command-spec
                                (plist-put (sget cmd) prop val)))
       (defgtp (x &rest props) (dolist (cmd (if (symbolp x) (list x) x))
                                 (let ((ls props))
                                   (while ls
                                     (jam cmd (car ls) (cadr ls))
                                     (setq ls (cddr ls)))))))
    (cl-macrolet ((deffull (who &body body)
                    (declare (indent 1))
                    `(defgtp ',who :full (lambda (sel)
                                           ,@body))))

      (deffull help
        (info "(gnugo)GTP command reference")
        (when sel (setq sel (intern (car sel))))
        (let (buffer-read-only pad cur spec output found)
          (cl-labels
              ((note (s) (insert pad "[NOTE: gnugo.el " s ".]\n")))
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^ *[*] \\([a-zA-Z_]+\\)\\(:.*\\)*\n"
                                        (point-max) t)
                (unless pad
                  (setq pad (make-string (- (match-beginning 1)
                                            (match-beginning 0))
                                         32)))
                (when (plist-get
                       (setq spec
                             (get (setq cur (intern (match-string 1)))
                                  :gnugo-gtp-command-spec))
                       :full)
                  (note "handles this command completely"))
                (when (setq output (plist-get spec :output))
                  (if (functionp output)
                      (note "handles the output specially")
                    (case output
                      (:discard (note "discards the output"))
                      (:message (note "displays the output in the echo area")))))
                (when (eq sel cur)
                  (setq found (match-beginning 0))))))
          (cond (found (goto-char found))
                ((not sel))
                (t (message "(no such command: %s)" sel)))))

      (deffull final_score
        (gnugo-display-final-score))

      (defgtp '(boardsize
                clear_board
                fixed_handicap)
        :output :discard
        :post-thunk (lambda ()
                      (gnugo--unclose-game)
                      (gnugo-put :last-mover nil)
                      (gnugo-refresh t)))

      (deffull loadsgf
        (gnugo-read-sgf-file (car sel)))

      (deffull (undo gg-undo)
        (gnugo-magic-undo
         (let (n)
           (cond ((not sel) 1)
                 ((cl-plusp (setq n (string-to-number (car sel)))) n)
                 (t (car sel)))))))))

(provide 'gnugo)


;;;---------------------------------------------------------------------------
;;; The remainder of this file defines a simplified SGF-handling library.
;;; When/if it should start to attain generality, it should be split off into
;;; a separate file (probably named sgf.el) w/ funcs and vars renamed sans the
;;; "gnugo/" prefix.

(defconst gnugo/sgf-*r4-properties*
  '((AB "Add Black"       setup list stone)
    (AE "Add Empty"       game  list point)
    (AN "Annotation"      game  simpletext)
    (AP "Application"     root  (simpletext . simpletext))
    (AR "Arrow"           -     list (point . point))
    (AS "Who adds stones" -     simpletext) ; (LOA)
    (AW "Add White"       setup list stone)
    (B  "Black"           move  move)
    (BL "Black time left" move  real)
    (BM "Bad move"        move  double)
    (BR "Black rank"      game  simpletext)
    (BT "Black team"      game  simpletext)
    (C  "Comment"         -     text)
    (CA "Charset"         root  simpletext)
    (CP "Copyright"       game  simpletext)
    (CR "Circle"          -     list point)
    (DD "Dim points"      -     elist point) ; (inherit)
    (DM "Even position"   -     double)
    (DO "Doubtful"        move  none)
    (DT "Date"            game  simpletext)
    (EV "Event"           game  simpletext)
    (FF "Fileformat"      root  [number (1 . 4)])
    (FG "Figure"          -     (or none (number . simpletext)))
    (GB "Good for Black"  -     double)
    (GC "Game comment"    game  text)
    (GM "Game"            root  [number (1 . 20)])
    (GN "Game name"       game  simpletext)
    (GW "Good for White"  -     double)
    (HA "Handicap"        game  number) ; (Go)
    (HO "Hotspot"         -     double)
    (IP "Initial pos."    game  simpletext) ; (LOA)
    (IT "Interesting"     move  none)
    (IY "Invert Y-axis"   game  simpletext)          ; (LOA)
    (KM "Komi"            game  real)                ; (Go)
    (KO "Ko"              move  none)
    (LB "Label"           -     list (point . simpletext))
    (LN "Line"            -     list (point . point))
    (MA "Mark"            -     list point)
    (MN "set move number" move  number)
    (N  "Nodename"        -     simpletext)
    (OB "OtStones Black"  move  number)
    (ON "Opening"         game  text)
    (OT "Overtime"        game  simpletext)
    (OW "OtStones White"  move  number)
    (PB "Player Black"    game  simpletext)
    (PC "Place"           game  simpletext)
    (PL "Player to play"  setup color)
    (PM "Print move mode" -     number) ; (inherit)
    (PW "Player White"    game  simpletext)
    (RE "Result"          game  simpletext)
    (RO "Round"           game  simpletext)
    (RU "Rules"           game  simpletext)
    (SE "Markup"          -     point)  ; (LOA)
    (SL "Selected"        -     list point)
    (SO "Source"          game  simpletext)
    (SQ "Square"          -     list point)
    (ST "Style"           root  [number (0 . 3)])
    (SU "Setup type"      game  simpletext) ; (LOA)
    (SZ "Size"            root  (or number (number . number)))
    (TB "Territory Black" -     elist point) ; (Go)
    (TE "Tesuji"          move  double)
    (TM "Timelimit"       game  real)
    (TR "Triangle"        -     list point)
    (TW "Territory White" -     elist point) ; (Go)
    (UC "Unclear pos"     -     double)
    (US "User"            game  simpletext)
    (V  "Value"           -     real)
    (VW "View"            -     elist point) ; (inherit)
    (W  "White"           move  move)
    (WL "White time left" move  real)
    (WR "White rank"      game  simpletext)
    (WT "White team"      game  simpletext)
    (LT "Lose on time"    setup simpletext))
  ;; r4-specific notes
  ;; - changed: DT FG LB RE RU SZ
  ;; - added: AP AR AS DD IP IY LN OT PM SE SQ ST SU VW
  "List of SGF[4] properties, each of the form (PROP NAME CONTEXT SPEC...).")

(defun gnugo/sgf-read-file (filename)
  "Return the collection (list) of gametrees in SGF[4] file FILENAME."
  (let ((keywords (or (get 'gnugo/sgf-*r4-properties* :keywords)
                      (put 'gnugo/sgf-*r4-properties* :keywords
                           (mapcar (lambda (full)
                                     (cons (car full)
                                           (intern (format ":%s" (car full)))))
                                   gnugo/sgf-*r4-properties*))))
        (specs (or (get 'gnugo/sgf-*r4-properties* :specs)
                   (put 'gnugo/sgf-*r4-properties* :specs
                        (mapcar (lambda (full)
                                  (cons (car full) (cdddr full)))
                                gnugo/sgf-*r4-properties*)))))
    (cl-labels
        ((sw () (skip-chars-forward " \t\n"))
         (x (end) (let ((beg (point))
                        (endp (case end
                                (:end (lambda (char) (= ?\] char)))
                                (:mid (lambda (char) (= ?\: char)))
                                (t (lambda (char) (or (= ?\: char)
                                                      (= ?\] char))))))
                        c)
                    (while (not (funcall endp (setq c (char-after))))
                      (cond ((= ?\\ c)
                             (delete-char 1)
                             (if (eolp)
                                 (kill-line 1)
                               (forward-char 1)))
                            ((looking-at "\\s-+")
                             (delete-region (point) (match-end 0))
                             (insert " "))
                            (t (forward-char 1))))
                    (buffer-substring-no-properties beg (point))))
         (one (type end) (let ((s (progn
                                    (forward-char 1)
                                    (x end))))
                           (case type
                             ((stone point move simpletext color) s)
                             ((number real double) (string-to-number s))
                             ((text) s)
                             ((none) "")
                             (t (error "Unhandled type: %S" type)))))
         (val (spec) (cond ((symbolp spec)
                            (one spec :end))
                           ((vectorp spec)
                            ;; todo: check range here.
                            (one (aref spec 0) :end))
                           ((eq 'or (car spec))
                            (let ((v (one (cadr spec) t)))
                              (if (= ?\] (char-after))
                                  v
                                (forward-char 1)
                                ;; todo: this assumes `spec' has the form
                                ;;         (or foo (foo . bar))
                                ;; i.e., foo is not rescanned.  e.g., `SZ'.
                                ;; probably this assumption is consistent
                                ;; w/ the SGF authors' desire to make the
                                ;; parsing easy, but you never know...
                                (cons v (one (cdaddr spec) :end)))))
                           (t (cons (one (car spec) :mid)
                                    (one (cdr spec) :end)))))
         (short (who) (when (eobp)
                        (error "Unexpected EOF while reading %s" who)))
         (atvalp () (= ?\[ (char-after)))
         (PROP () (let (name spec ltype)
                    (sw) (short 'property)
                    (when (looking-at "[A-Z]")
                      (setq name (read (current-buffer))
                            spec (cdr (assq name specs)))
                      (sw)
                      (cons
                       (cdr (assq name keywords))
                       (prog1 (if (= 1 (length spec))
                                  (val (car spec))
                                (unless (memq (setq ltype (car spec))
                                              '(elist list))
                                  (error "Bad spec: %S" spec))
                                (if (and (eq 'elist ltype) (sw)
                                         (not (atvalp)))
                                    nil
                                  (let ((type (cadr spec))
                                        mo ls)
                                    (while (and (sw) (atvalp)
                                                (setq mo (val type)))
                                      (push mo ls)
                                      (forward-char 1))
                                    (forward-char -1)
                                    (nreverse ls))))
                         (forward-char 1))))))
         (seek (c) (and (sw) (not (eobp)) (= c (char-after))))
         (seek-into (c) (when (seek c)
                          (forward-char 1)
                          t))
         (NODE () (when (seek-into ?\;)
                    (loop with prop
                          while (setq prop (PROP))
                          collect prop)))
         (TREE (lev) (prog1
                         ;; hmm
                         ;;  ‘append’ => ([NODE...] [SUBTREE...])
                         ;;  ‘cons’   => (([NODE...]) . [SUBTREE...])
                         (append
                          ;; nodes
                          (loop while (seek ?\;)
                                collect (NODE))
                          ;; subtrees
                          (loop while (seek-into ?\()
                                collect (TREE (1+ lev))))
                       (unless (zerop lev)
                         (assert (seek-into ?\)))))))
      (with-temp-buffer
        (insert-file-contents filename)
        (TREE 0)))))

(defun gnugo/sgf-write-file (collection filename)
  ;; take responsibility for our actions
  (dolist (tree collection)
    (let* ((root (car tree))
           (who (assq :AP root))
           (fruit (cons "gnugo.el" gnugo-version)))
      (if who
          (setcdr who fruit)
        (setcdr (last root) (list (cons :AP fruit))))))
  ;; write it out
  (let ((aft-newline-appreciated '(:AP :GN :PB :PW :HA :KM :RU :RE))
        (specs (mapcar (lambda (full)
                         (cons (intern (format ":%s" (car full)))
                               (cdddr full)))
                       gnugo/sgf-*r4-properties*))
        p name v spec)
    ;; todo: escape special chars for `text' and `simpletext'.
    (cl-labels
        ((>>one (v) (insert (format "[%s]" v)))
         (>>two (v) (insert (format "[%s:%s]" (car v) (cdr v))))
         (>>nl () (cond ((memq name aft-newline-appreciated)
                         (insert "\n"))
                        ((< 60 (current-column))
                         (save-excursion
                           (goto-char p)
                           (insert "\n")))))
         (>>prop (prop)
                 (setq p (point)
                       name (car prop)
                       v (cdr prop))
                 (insert (substring (symbol-name name) 1))
                 (cond ((not v))
                       ((and (consp v)
                             (setq spec (cdr (assq name specs)))
                             (memq (car spec)
                                   '(list elist)))
                        (>>nl)
                        (let ((>> (if (consp (cadr spec))
                                      #'>>two
                                    #'>>one)))
                          (dolist (little-v v)
                            (setq p (point))
                            (funcall >> little-v)
                            (>>nl))))
                       ((consp v)
                        (>>two v) (>>nl))
                       (t
                        (>>one v) (>>nl))))
         (>>node (node)
                 (loop initially (insert ";")
                       for prop in node
                       do (>>prop prop)))
         (>>tree (tree)
                 (unless (zerop (current-column))
                   (newline))
                 (insert "(")
                 (dolist (node tree)
                   (>>node node))
                 (insert ")")))
      (with-temp-buffer
        (dolist (tree collection)
          (>>tree tree))
        (newline)
        (write-file filename)))))

;;; gnugo.el ends here
