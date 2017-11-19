;;; paced.el --- Predictive Abbreviation Completion and Expansion using Dictionaries -*- lexical-binding: t; -*-

;; Copyright (c) 2017 Ian Dunn

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: convenience, completion
;; Version: 1.0
;; Created: 22 Jan 2017
;; Modified: 10 Nov 2017

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Paced scans one or more files or buffers and constructs a table of words,
;; weighted by how often they're used.

;; Once it's constructed this table, it can present them to the user for
;; completion, sorted by their weights.

;; Creating a new dictionary is easy; just use `paced-create-new-dictionary' to
;; create a new dictionary, then set the population commands and sort method.

;; Dictionaries are persistent; they're saved with
;; `paced-save-named-dictionary' or `paced-save-all-dictionaries'.

;; Population commands tell a dictionary how it should refresh its usage table.
;; The different types of populators are documented in `paced--populator-alist'.

;; No completion frontend is provided, but a function for
;; `completion-at-point-functions' is given.

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'thingatpt)
(require 'map)
(require 'eieio-base)
(require 'rx)

(defgroup paced nil
  "Predictive Abbreviation Completion and Expansion using Dictionaries"
  :group 'convenience)

(defcustom paced-thing-at-point-constituent 'symbol
  "Symbol defining THING which function `paced-mode' works on.
This symbol should be understandable by
`bounds-of-thing-at-point'.  This symbol defines what function `paced-mode'
considers to be the basic unit of expansion.  If if it set to `symbol',
for example, \"paced-mode\" would be offered as an expansion, while
if it is set to `word' \"paced\" and \"mode\" would be offered."
  :group 'paced
  :type 'symbol
  :options '(symbol word))

(defcustom paced-completion-ignore-case t
  "Non-nil to ignore case when completing.

Note that this does not affect dictionary population."
  :group 'paced
  :type 'boolean)

(defcustom paced-dictionary-directory (locate-user-emacs-file "paced-dictionaries/")
  "Directory in which the dictionaries are saved.

This is only used in `paced-load-all-dictionaries', so it's up to
the user whether to save dictionaries here."
  :group 'paced
  :type 'directory)

(defcustom paced-dictionary-directory-whitelist-regexp ".*"
  "Regexp to match when reading from the dictionary directory.

Any files that match this regexp will be loaded by
`paced-load-all-dictionaries'."
  :group 'paced
  :type 'regexp)

(defcustom paced-dictionary-directory-blacklist-regexp "$^"
  "Regexp to match for files NOT to load with `paced-load-all-dictionaries'.

This is the string \"$^\" by default, which matches nothing, thus
allowing all files."
  :group 'paced
  :type 'regexp)

(defcustom paced-load-all-dictionaries-recursively nil
  "Whether to recursively load all files with `paced-load-all-dictionaries'."
  :group 'paced
  :type 'boolean)

(defcustom paced-repopulate-saves-dictionary t
  "Whether to save a dictionary after repopulation."
  :group 'paced
  :type 'boolean)



(defun paced--default-dictionary-sort-func (usage-hash)
  "Default dictionary sort function.

Sort hash-table USAGE-HASH by the weights (values) in the table."
  ;; Unfortunately, there's no way to sort a hash-table, so we first convert it
  ;; into an alist, and sort that.
  (let ((seq (map-into usage-hash 'list)))
    (setq seq
          (seq-sort
           (pcase-lambda (`(_ . ,usage-lhs)
                          `(_ . ,usage-rhs))
             (> usage-lhs usage-rhs))
           seq))
    (map-into seq 'hash-table)))

(defclass paced-dictionary (eieio-named eieio-persistent)
  ((object-name :initarg :object-name
                :documentation "Symbol to use to refer to this dictionary.")
   (usage-hash :initarg :usage-hash
               :initform (make-hash-table :test #'equal)
               :type hash-table
               :documentation "Stores the usage data for this dictionary.")
   (population-commands
    :initarg :population-commands
    :initform nil
    :type (list-of paced-population-command)
    :custom (repeat (object :objectcreatefcn paced-new-population-command-custom))
    :label "Population Commands"
    :documentation "Commands to use when populating this dictionary.

Each entry must be a list of the form (TYPE ARGS PROPS).

TYPE is the type of populator; see `paced--populator-alist' for
allowed entries here.

ARGS is a list of arguments to pass to the populator, which are
specific to the given populator.

PROPS is a list of variables to let-bind when populating.  Each
entry should be of the form (VAR VALUE).")
   (file-header-line :type string
		     :allocation :class
		     :initform ";; Paced Dictionary"
		     :documentation
		     "Header line for the save file.
This is used with the `object-write' method.")
   (case-sensitive :initarg :case-sensitive
                   :initform nil
                   :type boolean
                   :custom boolean
                   :label "Case Sensitive"
                   :documentation "Non-nil if the dictionary should be case sensitive when populating.

When nil, \"This\" will be the same as \"this\" in `usage-hash'.")
   (updated :initarg :updated
            :initform nil
            :type boolean
            :documentation "Non-nil if this dictionary has been updated since it was last saved.")
   (sort-method :initarg :sort-method
                :initform 'paced--default-dictionary-sort-func
                :type function
                :label "Sort Method"
                :custom function
                :documentation "Method by which this dictionary should sort its usage table.

This should be a function of one argument, the usage-hash slot,
and return a sorted hash-table.

This defaults to `paced--default-dictionary-sort-func'."))
  "Paced dictionary.")

(defvar paced--registered-dictionaries nil
  "Internal list of registered dictionaries.

Do not edit this list manually.  Use `paced-make-dictionary'
instead.")

(defsubst paced-named-dictionary (key)
  (map-elt paced--registered-dictionaries key nil 'equal))

(defsubst paced-dictionary-names ()
  (map-keys paced--registered-dictionaries))

(defsubst paced-read-dictionary ()
  (completing-read "Dictionary: " (map-keys paced--registered-dictionaries)))

(defsubst paced-dictionary-registered-p (key)
  (map-contains-key paced--registered-dictionaries key 'equal))

(defsubst paced-ensure-registered (key)
  (unless (paced-dictionary-registered-p key)
    (error "No paced dictionary called '%s' has been registered." key)))

(defsubst paced-register-dictionary (key dict)
  (map-put paced--registered-dictionaries key dict 'equal))

(defsubst paced--ensure-dictionary-directory ()
  (make-directory paced-dictionary-directory t))

(defun paced-make-dictionary (name filename case-sensitive)
  "Make a paced dictionary called NAME.

NAME is a symbol used to identify the new dictionary.

If a paced dictionary is already registered with name NAME, then
it is replaced with a new, empty one.

Return value is the new dictionary."
  (let ((new-dict (paced-dictionary
                   :object-name name
                   :file filename
                   :case-sensitive case-sensitive)))
    (paced-register-dictionary name new-dict)
    new-dict))

(defun paced-create-new-dictionary (name file)
  "Create a new dictionary called NAME.

FILE is the file in which to store the new dictionary.

Once named, the dictionary can be edited through the EIEIO
customization interface."
  (declare (interactive-only paced-make-dictionary))
  (interactive (list (read-string "Name: ")
                     (read-file-name "Storage File: " paced-dictionary-directory)))
  (let ((new-dict (paced-dictionary :object-name name
                                    :file file)))
    (customize-object new-dict)))

(cl-defmethod paced-dictionary-name ((obj paced-dictionary))
  (oref obj object-name))

(defcustom paced-global-dict-enable-alist nil
  "Global enable list.

Each entry has the form (CONDITION . DICT-KEY), where CONDITION
is one of the following forms:

- A mode name, such as `org-mode' or `text-mode', indicating that
  the named dictionary should be active in any mode derived from
  that mode.

- A symbol, in which case the named dictionary is active whenever
  the value of that symbol is non-nil.

- A function symbol, in which case the function is called with no
  arguments to determine if the given dictionary should be
  enabled.  If the function returns non-nil the dictionary is enabled.

- A lambda function, in which case it is called with no
  arguments, and if it returns non-nil, the dictionary is
  enabled.

- The form (or CONDITION1 CONDITION2 ...), which enables the
  given dictionary if any of the conditions are met.

- The form (and CONDITION1 CONDITION2 ...), which enables the
  given dictionary if all of the conditions are met.

No matter what this list indicates, dictionaries will not be
enabled unless paced-mode is active."
  :group 'paced
  :type '(alist :key-type sexp :value-type sexp))

(defvar-local paced-local-dict-enable-alist nil
  "Local enable list.

Has the same form as and takes priority over
`paced-global-dict-enable-alist'.")

(defun paced-dict-enable-list ()
  (append paced-local-dict-enable-alist
          paced-global-dict-enable-alist))

(defun paced-test-dict-enable-condition (condition)
  "Determines if CONDITION passes in the current buffer.

See `paced-global-dict-enable-alist' for an explanation."
  (pcase condition
    ((and (pred symbolp)
          (app symbol-name (rx "-mode" string-end)))
     (derived-mode-p condition))
    ((and (pred symbolp)
          (pred boundp))
     (symbol-value condition))
    ((and (pred symbolp)
          (pred fboundp))
     (funcall condition))
    ((pred functionp)
     (funcall condition))
    (`(or . ,rest)
     (seq-some 'paced-test-dict-enable-condition rest))
    (`(and . ,rest)
     (seq-every-p 'paced-test-dict-enable-condition rest))))

(defun paced-current-dictionary ()
  "Determine the current dictionary.

Returns nil if no dictionary should be enabled.

If a dictionary is found in the list that doesn't exist, it will
be skipped."
  (let ((conditions (paced-dict-enable-list))
        (dictionary))
    (while (and conditions
                (not dictionary))
      (pcase-let* ((`(,condition . ,dict) (pop conditions)))
        (when (and (paced-dictionary-registered-p dict)
                   (paced-test-dict-enable-condition condition))
          (setq dictionary dict))))
    (when dictionary
      (paced-named-dictionary dictionary))))

(cl-defmethod paced-save-dictionary ((dict paced-dictionary))
  "Save dictionary DICT according to its filename."
  (when (oref dict updated)
    (eieio-persistent-save dict))
  (oset dict updated nil))

(defun paced-save-named-dictionary (key)
  "Save dictionary named KEY."
  (declare (interactive-only paced-save-dictionary))
  (interactive (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-save-dictionary dict)))

(defun paced-load-dictionary-from-file (file)
  "Load dictionary from FILE."
  (interactive
   (list (read-file-name "Dictionary File: " paced-dictionary-directory)))
  (when-let* ((new-dict (eieio-persistent-read file 'paced-dictionary)))
    (paced-register-dictionary
     (paced-dictionary-name new-dict)
     new-dict)))

(defun paced-save-all-dictionaries ()
  "Save all registered dictionaries."
  (interactive)
  (map-apply
   (lambda (_ dict)
     (paced-save-dictionary dict))
   paced--registered-dictionaries))

;;;###autoload
(defun paced-load-all-dictionaries ()
  "Load all dictionaries in `paced-dictionary-directory'."
  (interactive)
  (message "Loading all dictionaries from %s" paced-dictionary-directory)
  (paced--ensure-dictionary-directory)
  (let ((files-to-load
         (if paced-load-all-dictionaries-recursively
             (directory-files-recursively paced-dictionary-directory
                                          paced-dictionary-directory-whitelist-regexp)
           (directory-files paced-dictionary-directory t
                            paced-dictionary-directory-whitelist-regexp))))
    (dolist (dict-file files-to-load)
      (when (and (file-regular-p dict-file)
                 (not (string-match-p paced-dictionary-directory-blacklist-regexp dict-file)))
        (paced-load-dictionary-from-file dict-file)))))

(cl-defmethod eieio-done-customizing ((dict paced-dictionary))
  (paced-register-dictionary (paced-dictionary-name dict) dict)
  (paced--ensure-dictionary-directory)
  (paced-save-dictionary dict))



(defvar-local paced--current-source nil
  "The source from which a dictionary is being populated.

This is used internally to inform the user of the current source,
since population mostly uses temporary buffers.")

(defvar-local paced-exclude-function (lambda () nil)
  "Local predicate to determine if thing at point should be excluded.

This should be a function of no arguments that returns non-nil if
the current thing-at-point should be excluded from paced dictionaries.

By default, this allows everything.")

(defun paced-excluded-p ()
  "Return non-nil to exclude current thing at point.

See `paced-exclude-function' for more."
  (funcall paced-exclude-function))

(defun paced-bounds-of-thing-at-point ()
  "Get the bounds of the thing at point."
  (bounds-of-thing-at-point paced-thing-at-point-constituent))

(defun paced-thing-at-point ()
  "Return the current thing at point.

The thing is determined by `paced-thing-at-point-constituent'.

Text properties are excluded."
  (when-let* ((bounds (paced-bounds-of-thing-at-point)))
    (buffer-substring-no-properties
     (car bounds) (cdr bounds))))

(defun paced-forward-thing (&optional number)
  "Move forward NUMBER things.

Things is based on `paced-thing-at-point-constituent'."
  (interactive "p")
  (forward-thing paced-thing-at-point-constituent number))

(defsubst paced-add-word-to-dict (dict word)
  "Add WORD to paced dictionary DICT."
  ;; If I've got a word uppercase and lowercase in my usage table, I'm
  ;; going to have repeats when ignore case is enabled.  To solve this,
  ;; downcase everything when not case sensitive.
  (let ((new-word (if (oref dict case-sensitive) word (downcase word))))
    ;; Use the full name here to silence the byte-compiler
    (cl-incf (map-elt (oref dict usage-hash) new-word 0))
    (oset dict updated t)))

(defsubst paced-add-word-to-current-dict (word)
  "Add WORD to the current paced dictionary."
  (if-let* ((dict (paced-current-dictionary)))
      (paced-add-word-to-dict dict word)
    (error "No dictionary found")))

(cl-defmethod paced-populate-dictionary-from-buffer ((dict paced-dictionary) &optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let* ((reporter-string
              (concat (format "Populating dictionary %s" (paced-dictionary-name dict))
                      (when paced--current-source (format " from %s"
                                                          paced--current-source))
                      "..."))
             (reporter (make-progress-reporter reporter-string 0 100)))
        (while (paced-forward-thing)
          (progress-reporter-do-update
           reporter
           (floor (* 100.0 (/ (float (point)) (point-max)))))
          (unless (paced-excluded-p)
            (paced-add-word-to-dict dict (paced-thing-at-point))))
        (progress-reporter-done reporter)))))

(defun paced-populate-dictionary-from-region (dict start end)
  "Populate DICT from the region in the current buffer between START and END.

Note that this doesn't add the current buffer to DICT's
population commands, so if DICT is later repopulated using
`paced-repopulate-dictionary' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (save-restriction
    (narrow-to-region start end)
    (paced-populate-dictionary-from-buffer dict)))

(defun paced-populate-buffer-dictionary (&optional buffer)
  "Populate BUFFER's current dictionary with BUFFER.

This means add a usage of each included thing in buffer.

If called interactively, the current buffer is used.  In order to
only populate the dictionary from a region,
`paced-populate-from-region'.

Note that this doesn't add BUFFER to the dictionary's population
commands, so if it is later repopulated using
`paced-repopulate-dictionary' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost.

In order to make changes permanent, use
`paced-add-buffer-file-to-dictionary'."
  (interactive)
  (if-let* ((dict (paced-current-dictionary)))
      (paced-populate-dictionary-from-buffer dict buffer)
    (user-error "No dictionary found")))

(defun paced-populate-from-region (start end)
  "Populate the current dictionary from the region START to END.

Note that this doesn't add the current buffer to the dictionary's
population commands, so if it is later repopulated using
`paced-repopulate-dictionary' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (interactive "r")
  (if-let* ((dict (paced-current-dictionary)))
      (paced-populate-dictionary-from-region dict start end)
    (user-error "No dictionary found")))

(defun paced-add-current-thing-to-dict ()
  "Add the current thing at point to the current dictionary.

No check is done to determine if the current thing should be
excluded.

Note that this doesn't add anything to the dictionary's
population commands, so if it is later repopulated using
`paced-repopulate-dictionary' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (interactive)
  (paced-add-word-to-current-dict (paced-thing-at-point)))

(cl-defmethod paced-reset-dictionary ((dict paced-dictionary))
  "Reset the usage-hash of paced-dictionary DICT."
  (oset dict usage-hash (oref-default dict usage-hash)))

(defun paced-reset-named-dictionary (key)
  "Reset the paced dictionary with key KEY."
  (interactive
   (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-reset-dictionary dict)))

(cl-defmethod paced-sort-dictionary ((dict paced-dictionary))
  (oset dict usage-hash
        (funcall (oref dict sort-method)
                 (oref dict usage-hash))))

(defun paced-sort-named-dictionary (key)
  (interactive (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-sort-dictionary dict)))



(define-minor-mode paced-mode
  "Toggle paced mode.

This adds `paced-completion-at-point' to
`completion-at-point-functions'."
  :init-value nil
  :lighter " paced"
  :group 'paced
  (if paced-mode
      (add-hook 'completion-at-point-functions 'paced-completion-at-point 'append 'local)
    (remove-hook 'completion-at-point-functions 'paced-completion-at-point 'local)))

(define-globalized-minor-mode global-paced-mode paced-mode paced-mode
  :group 'paced)


                                        ; ;;;;;;;;;;;;;;;; ;
                                        ; ;; Completion ;; ;
                                        ; ;;;;;;;;;;;;;;;; ;

(cl-defmethod paced-dictionary-completions-for-prefix ((dict paced-dictionary) prefix)
  (let* ((completion-ignore-case paced-completion-ignore-case)
         (prefix-length (length prefix)))
    ;; Account for case differences in the prefix by prepending the prefix.
    (mapcar
     (lambda (completion)
       (when (stringp completion)
         (concat prefix (substring-no-properties completion prefix-length))))
     (all-completions prefix (oref dict usage-hash)))))

(defun paced-completions-for-prefix (prefix)
  "Get completions for PREFIX from the current dictionary."
  (if-let* ((dict (paced-current-dictionary)))
      (paced-dictionary-completions-for-prefix dict prefix)
    (user-error "No dictionary found")))

(defun paced-completion-finish (prefix completions)
  "Account for case differences in the prefix by prepending the prefix."
  (cond
   ((not (listp completions))
    ;; If completions is not a list, it's likely 't', in which
    ;; case just return the original prefix.
    (list prefix))
   (t
    (let ((prefix-length (length prefix)))
      (mapcar
       (lambda (completion)
         (when (stringp completion)
           (concat prefix (substring-no-properties completion prefix-length))))
       completions)))))

(defun paced-completion-table-function (string pred action)
  (if-let* ((dict (paced-current-dictionary)))
      (let* ((completion-ignore-case paced-completion-ignore-case))
        (pcase action
          (`nil
           (paced-completion-finish string
                                    (try-completion string (oref dict usage-hash) pred)))
          (`t
           (paced-completion-finish string
                                    (all-completions string (oref dict usage-hash) pred)))
          (`lambda
            (paced-completion-finish string
                                     (test-completion string (oref dict usage-hash) pred)))
          (`(boundaries . _) nil)
          (`metadata
           `(metadata . ((category . paced)
                         (annotation-function . nil)
                         (display-sort-function . identity)
                         (cycle-sort-function . identity))))))
    (user-error "No dictionary found for current buffer")))

(defcustom paced-auto-update-p nil
  "Whether to update from completions.

This only works for an existing entry."
  :group 'paced
  :type 'boolean)

(defun paced-completion-auto-update (word status)
  (cl-case status
    (sole
     ;; We're done with completion, but the user may still be typing.
     ;; Therefore, don't add it.
     )
    (exact
     ;; Might not be the entire completion, so don't add it.
     )
    (finished
     (when paced-auto-update-p
       (paced-add-word-to-current-dict word)))))

(defun paced-completion-at-point ()
  "Function for `completion-at-point-functions' to get the paced completions"
  ;; Don't expand unless we're in a buffer with paced-mode enabled.
  (when (and paced-mode)
    (when-let* ((bounds (paced-bounds-of-thing-at-point)))
      (list (car bounds) (cdr bounds) 'paced-completion-table-function
            :exit-function 'paced-completion-auto-update))))


                                        ; ;;;;;;;;;;;;;;;;;; ;
                                        ; ;; Repopulation ;; ;
                                        ; ;;;;;;;;;;;;;;;;;; ;

(defun paced--insert-file-contents (file)
  "Inserts the contents of FILE into the current buffer.

Unlike `insert-file-contents', this handles mode hooks, which
paced requires for repopulation (syntax tables, exclude functions, etc.).

Returns nil if FILE doesn't exist."
  (if (not (file-exists-p file))
      (progn (message "Paced couldn't find file %s" file) nil)
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (after-find-file))
    t))

(defclass paced-population-command ()
  ((props :initarg :props
          :initform nil
          :type list
          :label "Properties"
          :custom (alist :tag "Options" :key-type variable :value-type sexp)
          :documentation "A list of variables to set during population.

Each element is of the form (VAR VALUE).")))

(cl-defmethod paced-population-command-prepare-props ((cmd paced-population-command))
  "Turn props of CMD into a form understood by `let'."
  (with-slots (props) cmd
    (map-apply (lambda (var val) (list var val)) props)))

(cl-defgeneric paced-population-command-source-list ((_cmd paced-population-command)))

(cl-defgeneric paced-population-command-setup-buffer ((_cmd paced-population-command) _source)
  "Prepare a temporary buffer with SOURCE.

Return non-nil if setup was successful and population can continue.")

(cl-defmethod paced-population-command-populate-dictionary ((dict paced-dictionary) (cmd paced-population-command))
  (let ((sources (paced-population-command-source-list cmd))
        ;; Turn props into a form understood by `let'.
        (props (paced-population-command-prepare-props cmd)))
    (dolist (source sources)
      (with-temp-buffer
        ;; If pre is nil, continue.
        ;; Otherwise, continue if pre returns non-nil
        ;; This allows users to specify conditions under which repopulation
        ;; should be disabled.
        (let ((paced--current-source source))
          (when (paced-population-command-setup-buffer cmd source)
            (eval (macroexp-let* props `(paced-populate-dictionary-from-buffer ,dict)))))))))

(defclass paced-file-population-command (paced-population-command)
  ((file :initarg :file
         :initform ""
         :type string
         :label "File"
         :custom (file :tag "File")
         :documentation "File from which to populate."))
  "Populates a dictionary from all words in a single file.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-population-command))
  (list (oref cmd file)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-file-population-command) source)
  (paced--insert-file-contents source))

(defclass paced-buffer-population-command (paced-population-command)
  ((buffer :initarg :buffer
           :initform ""
           :type string
           :label "Buffer"
           :custom (string :tag "Buffer")
           :documentation "Name of the buffer from which to populate."))
  "Populates a dictionary from all words in a given buffer.

That buffer must be a string, and must exist during population.")

(cl-defmethod paced-population-command-source-list ((cmd paced-buffer-population-command))
  (list (oref cmd buffer)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-buffer-population-command) source)
  (cond
   ((not (stringp source))
    (message "Paced buffer populator got an invalid argument: %s" source)
    nil)
   ((not (get-buffer source))
    (message "Paced buffer populator got a buffer that doesn't exist: %s" source)
    nil)
   (t
    (set-buffer source))))

(defclass paced-file-function-population-command (paced-population-command)
  ((file :initarg :file
         :initform ""
         :type string
         :label "File"
         :custom (file :tag "File")
         :documentation "File from which to populate.")
   (setup-func :initarg :setup-func
               :initform (lambda () t)
               :type function
               :label "Setup Function"
               :custom (function :tag "Setup Function")
               :documentation "Additional setup function."))
  "Populate from a given file, using a setup function.

That function is called with no arguments, with a temporary
buffer containing the file's contents, and must return non-nil if
population may continue.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-function-population-command))
  (list (oref cmd file)))

(cl-defmethod paced-population-command-setup-buffer ((cmd paced-file-function-population-command) source)
  (and (paced--insert-file-contents source)
       (funcall (oref cmd setup-func))))

(defclass paced-directory-regexp-population-command (paced-population-command)
  ((directory :initarg :directory
              :initform ""
              :type string
              :label "Directory"
              :custom (directory :tag "Directory")
              :documentation "Directory to search for files from which to populate.")
   (regexp :initarg :regexp
           :initform ".*"
           :type string
           :label "File Regexp"
           :custom (string :tag "File Regexp")
           :documentation "Regular expression to match files.")
   (recursive :initarg :recursive
              :initform t
              :type boolean
              :label "Recursive"
              :custom boolean
              :documentation "Whether to search through the directory recursively."))
  "Population command to populate from files in a directory that
match a regular expression.")

(cl-defmethod paced-population-command-source-list ((cmd paced-directory-regexp-population-command))
  (with-slots (directory regexp recursive) cmd
    (if recursive
        (directory-files-recursively directory regexp)
      (directory-files directory t regexp))))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-directory-regexp-population-command) source)
  (paced--insert-file-contents source))

(defclass paced-file-list-population-command (paced-population-command)
  ((generator :initarg :generator
              :initform (lambda () nil)
              :type function
              :label "Generator"
              :custom (function :tag "Generator")
              :documentation "Function of no arguments that returns a list of files."))
  "Populate a dictionary from a list of files.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-list-population-command))
  (funcall (oref cmd generator)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-file-list-population-command) source)
  (paced--insert-file-contents source))

(defun paced-new-population-command-custom ()
  (let* ((type (completing-read "Command Type: "
                                (eieio-class-children 'paced-population-command))))
    (funcall (intern type))))

(cl-defmethod paced-repopulate-dictionary ((dict paced-dictionary))
  "Repopulate dictionary DICT from its population commands.

Population commands are stored in the field of the same name.

Note that this will empty the dictionary's contents before
repopulating it."
  ;; Empty the dictionary
  (paced-reset-dictionary dict)
  (dolist (cmd (oref dict population-commands))
    (paced-population-command-populate-dictionary dict cmd))
  (paced-sort-dictionary dict)
  (when paced-repopulate-saves-dictionary
    (paced-save-dictionary dict)))

(defun paced-repopulate-named-dictionary (key)
  "Repopulate dictionary named KEY from its population commands.

Population commands are stored in the field of the same name.

Note that this will empty the dictionary's contents before
repopulating it."
  (interactive
   (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    ;; TODO: Warn about reset.
    (paced-repopulate-dictionary dict)))

(defun paced-add-buffer-file-to-dictionary (&optional buffer)
  "Populate the dictionary of BUFFER with BUFFER.

The file corresponding to BUFFER is then added to the current
dictionary's population commands.

Custom settings for the populator, such as the exclude function,
must be set with `paced-edit-named-dictionary' or
`paced-edit-current-dictionary'."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (buffer-file-name)
      (user-error "paced-add-buffer-file-to-dictionary called inside a non-file buffer."))
    (if-let* ((dict      (paced-current-dictionary))
              (file-name (buffer-file-name))
              (cmd (paced-file-population-command :file file-name)))
        (progn
          (paced-populate-dictionary-from-buffer dict buffer)
          (cl-pushnew cmd (oref dict population-commands) :test 'equal))
      (user-error "No dictionary found for current buffer."))))



(defun paced-edit-named-dictionary (name)
  "Edit the paced-dictionary named NAME."
  (interactive (list (paced-read-dictionary)))
  (if-let* ((dict (paced-named-dictionary name)))
      (customize-object dict)
    (error "No paced dictionary called '%s' has been registered." name)))

(defun paced-edit-current-dictionary ()
  "Edit the current paced dictionary."
  (interactive)
  (if-let* ((dict (paced-current-dictionary)))
      (customize-object dict)
    (user-error "No dictionary found for current buffer")))

(provide 'paced)

;;; paced.el ends here
