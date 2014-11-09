;;; enwc.el --- The Emacs Network Client

;; Copyright (C) 2012,2013,2014 Free Software Foundation, Inc.

;; Author: Ian Dunn
;; Keywords: enwc, network, wicd, manager, nm

;; This file is part of ENWC

;; ENWC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; ENWC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ENWC; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;; connect
;; disconnect
;; scan
;; get-network-prop
;; get-profile-prop
;; save-profile
;; get-networks
;; check-connecting
;; get-current-nw-id

;;; Commentary:
;; In order to use this, add
;;
;; (require 'enwc-setup)
;; (enwc-setup)
;;
;; to your .emacs file.

(require 'dbus)
(require 'wid-edit)
(require 'tabulated-list)
(require 'cl-lib)
(require 'cl-macs)
(require 'format-spec)

;;; Code:

(defgroup enwc nil
  "*The Emacs Network Client"
  :prefix "enwc-"
  :group 'applications)

(defcustom enwc-wireless-device "wlan0"
  "The wireless device to use for ENWC."
  :group 'enwc
  :type 'string)

(defcustom enwc-wired-device "eth0"
  "The wired device to use for ENWC."
  :group 'enwc
  :type 'string)

(defcustom enwc-backends '(wicd nm)
  "The list of backends to be used by ENWC.
These will be checked in the order designated here,
and the first active backend found will be used."
  :group 'enwc
  :type 'list)

(defcustom enwc-display-mode-line 't
  "Whether or not to use ENWC's modeline display.
When set to non-nil, this will cause the current
network signal strength to be displayed on the
Emacs mode line."
  :group 'enwc
  :type 'boolean)

(defcustom enwc-auto-scan 't
  "Whether or not to have ENWC automatically scan.
If non-nil, then ENWC will automatically scan for
networks every `enwc-auto-scan-interval' seconds."
  :group 'enwc
  :type 'boolean)

(defcustom enwc-auto-scan-interval 20
  "The interval between automatic scans."
  :group 'enwc
  :type 'integer)

(defcustom enwc-mode-line-format "[%s%%]"
  "The format for displaying the mode line.

%s = The current signal strength.  If wired, then this is set to 100.

%e = The essid of the current network.  If wired, then this set to 'Wired'

%b = The bssid of the current network.  If using a wired connection, then this
is set to 'Wired'.

%n = The encryption type of the current network, or 'Wired' if using a wired
connection.

%c = The channel of the current network, or 'Wired' if using a wired connection.

%% = A Normal '%'"
  :group 'enwc
  :type 'string)

;;; The function variables for the abstract layer.

(defvar enwc-scan-func nil
  "The function variable for the scan function.
This variable is set during setup.")

(defvar enwc-get-networks-func nil
  "A function variable to be used in `enwc-get-networks'.
This is redefined during setup to be the function to get the network
 list.")

(defvar enwc-get-wireless-nw-props-func nil)

(defvar enwc-connect-func nil
  "The function variable for the connect function.")

(defvar enwc-disconnect-func nil
  "The function variable for the disconnect function.")

(defvar enwc-get-current-nw-id-func nil
  "The function variable to be used in `enwc-get-current-nw-id'.
This is redefined during setup to be the function to get
the current network id.")

(defvar enwc-check-connecting-func nil
  "The function variable to be used in `enwc-check-connecting'.
This is redefined during setup to be the function to
check whether or not ENWC is connecting.")

(defvar enwc-is-wired-func nil
  "The function variable to be used in `enwc-is-wired'.
This is redefined during setup to be the function to
check whether or not a wired connection is active.")

(defvar enwc-get-profile-info-func nil)

(defvar enwc-save-nw-settings-func nil
  "The function variable to be used in `enwc-save-nw-settings'.
This is redefined during setup to be the function to save
the network settings of a given network.")

(defvar enwc-display-string " [0%] "
  "The mode line display string.
This is altered every second to display the current network strength
in `enwc-update-mode-line'.")

(defun enwc-print-strength (s)
  "Convert signal strength S to a string to dispay."
  (concat (number-to-string s) "%"))

(defvar enwc-details-alist
  `((strength . ((display . "Str")
                 (width   . 0)
                 (conv    . enwc-print-strength)))
    (essid    . ((display . "Essid")
                 (width   . 0)
                 (conv    . identity)))
    (encrypt  . ((display . "Encrypt")
                 (width   . 0)
                 (conv    . identity)))
    (bssid    . ((display . "Bssid")
                 (width   . 0)
                 (conv    . identity)))
    (channel  . ((display . "Chnl")
                 (width   . 0)
                 (conv    . identity))))
  "Alist of the details to their information.

The information entries themselves are an alist, with the following keys:

DISPLAY is the display name.
WIDTH is used during display to keep track of the width of each one.
CONV is the conversion function used during display.")

(defvar enwc-last-scan nil
  "The most recent scan results.

This will be an association list of the form:

((ID . ((strength . STRENGTH) (essid . ESSID) ...)) ...)

Each ID is a backend-specific network ID.

Each key in the children association lists corresponds to an entry in
`enwc-details-alist'.")

(defvar enwc-access-points nil
  "The most recent access point list.")

(defvar enwc-using-wired nil
  "Whether or not wired mode is active.

This is `non-nil' if ENWC is using wired connections.
Note that this is NOT the same as `enwc-is-wired'.  This checks
whether or not ENWC is in wired mode.")

(defvar enwc-scan-done nil
  "Whether or not a scan is finished.")

(defvar enwc-edit-id nil
  "This is the network id of the network being edited.")

(defvar enwc-scan-requested nil
  "Indicates that a scan has been requested.
This is used so as to avoid multiple updates of the scan data.")

(defvar enwc-scan-interactive nil
  "Indicates that a scan was interactively requested.
This is only used internally.")

(defvar enwc-display-mode-line-timer nil
  "The timer that updates the mode line display.")

(defvar enwc-scan-timer nil
  "The timer for automatic scanning.")

(make-local-variable 'enwc-edit-id)

;; The Faces

(defface enwc-connected-face
  '((((class color) (background dark))
     (:foreground "Green"))
    (((class color) (background light))
     (:foreground "Green"))
    (t (:background "Green")))
  "The face for the connected network."
  :group 'enwc)

(eval-when-compile
  (defun enwc--break-by-words (str)
    "Break up string STR into a list of words."
    (cl-check-type str string)
    (split-string str "-\\|_\\| "))

  (defun enwc--sym-to-str (sym &optional seps)
    "Create a string from symbol SYM.
SEPS is a string specifying the separator to use to combine the words,
or \" \" if not specified."
    (cl-check-type sym symbol)
    (unless seps
      (setq seps " "))
    (cl-check-type seps string)
    (combine-and-quote-strings (enwc--break-by-words (symbol-name sym)) seps))

  (defun enwc--str-to-sym (str &optional seps)
    "Create a symbol from the string STR.
This will break STR into words, and then put it back together separating
each word by SEPS, which defaults to \"-\"."
    (cl-check-type str string)
    (unless seps
      (setq seps "-"))
    (cl-check-type seps string)
    (intern (combine-and-quote-strings (enwc--break-by-words str) seps))))

(defun enwc--int-to-byte-list (n)
  "Convert 32-bit integer N into a byte list."
  (cl-check-type n integer)
  (let (ret)
    (dotimes (x 4 ret)
      (push (logand n 255) ret)
      (setq n (lsh n -8)))))

(defun enwc--byte-list-to-int (bl)
  "Convert byte list BL into a 32-bit integer."
  (cl-check-type bl list)
  (let ((ret 0))
    (dolist (x bl ret)
      (setq ret (logior (lsh ret 8) x)))))

(defun enwc--htonl (n)
  "Convert 32-bit integer N from hardware to network byte order."
  (cl-check-type n integer)
  (enwc--byte-list-to-int (nreverse (enwc--int-to-byte-list n))))

;;;;;;;;;;;;;;;;;;;;
;; ENWC functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun enwc-get-networks ()
  "Gets the identifiers for the access points
from a previous scan."
  (funcall enwc-get-networks-func))

(defun enwc-do-scan ()
  "Runs a backend scan."
  (funcall enwc-scan-func))

(defun enwc-connect (id)
  "Connect to network with id ID.

ID is specific to the backend."
  (funcall enwc-connect-func id enwc-using-wired))

(defun enwc-disconnect ()
  "Disconnect from the current network."
  (funcall enwc-disconnect-func enwc-using-wired))

(defun enwc-get-current-nw-id ()
  "Gets the id of the current network id,
or nil if there isn't one.

The returned id is specific to the backend."
  (funcall enwc-get-current-nw-id-func enwc-using-wired))

(defun enwc-check-connecting-p ()
  "Check to see if there is a connection in progress.
Returns `non-nil' if there is one, nil otherwise."
  (funcall enwc-check-connecting-func))

(defun enwc-get-wireless-nw-props (id)
  "Get the network properties of the wireless network with id ID.
This will return an associative list with the keys
corresponding to `enwc-details-alist'.

ID is specific to the backend."
  (funcall enwc-get-wireless-nw-props-func id))

(defun enwc-is-wired-p ()
  "Checks whether or not ENWC is connected to a wired network.
Note that this is NOT the same as `enwc-using-wired'.
This checks for an active wired connection."
  (funcall enwc-is-wired-func))

(defun enwc-get-profile-info (id)
  "Get the profile information for network ID.

ID is specific to the backend."
  (funcall enwc-get-profile-info-func id enwc-using-wired))

(defun enwc-save-nw-settings (id settings)
  "Saves network settings SETTINGS to the network profile with network id ID.
SETTINGS is an association list with entries for the IP Address, Netmask,
Gateway, DNS Servers, and Security.  WIRED is set to indicate whether or not
this is a wired network.

ID is specific to the backend."
  (funcall enwc-save-nw-settings-func id settings enwc-using-wired))

;;;;;;;;;;;;;;;;;;;;;;
;; Actual Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-is-valid-nw-id-p (id)
  "Confirms that ID is a valid network id."
  (and id (not (eq id 'wired))))

(defun enwc-value-from-scan (detail &optional id)
  "Retrieve a value for DETAIL from `enwc-last-scan'.
If ID is specified, then it will get the entry for ID.
Otherwise, ID is set to the current network ID.

If DETAIL is not found in `enwc-last-scan', then return nil."
  (unless id
    (setq id (enwc-get-current-nw-id)))
  (when enwc-last-scan
    (alist-get detail (alist-get id enwc-last-scan))))

(defun enwc-make-format-spec ()
  (let* ((cur-id (enwc-get-current-nw-id))
         (wiredp (enwc-is-wired-p))
         (no-last (not enwc-last-scan))
         (invalid-id (not (enwc-is-valid-nw-id-p cur-id)))
         (connectingp (enwc-check-connecting-p)))
    (format-spec-make ?s (cond
                          (wiredp "100")
                          ((or invalid-id no-last) "0")
                          (connectingp "*")
                          (t (enwc-value-from-scan 'strength cur-id)))
                      ?e (cond
                          (wiredp "Wired")
                          ((or invalid-id connectingp no-last) "None")
                          (t (enwc-value-from-scan 'essid cur-id)))
                      ?b (cond
                          (wiredp "Wired")
                          ((or invalid-id connectingp no-last) "none")
                          (t (enwc-value-from-scan 'bssid cur-id)))
                      ?n (cond
                          (wiredp "None")
                          ((or invalid-id connectingp no-last) "None")
                          (t (enwc-value-from-scan 'encrypt cur-id)))
                      ?c (cond
                          (wiredp "None")
                          ((or invalid-id connectingp no-last) "None")
                          (t (enwc-value-from-scan 'channel cur-id)))
                      ?% "%")))

(defun enwc-format-mode-line-string ()
  "Formats the mode line string.
This is derived from `enwc-mode-line-format'.
See the documentation for it for more details."
  (format-spec enwc-mode-line-format (enwc-make-format-spec)))

(defun enwc-update-mode-line ()
  "Updates the mode line display.
This uses the format specified by `enwc-mode-line-format'.
This is initiated during setup, and runs once every second."
  (setq enwc-display-string (enwc-format-mode-line-string))
  (force-mode-line-update))

(defun enwc-enable-display-mode-line ()
  "Enables the mode line display."
  (interactive)
  (or global-mode-string (setq global-mode-string '("")))
  (setq enwc-display-mode-line t)
  (unless (member 'enwc-display-string global-mode-string)
    (setq global-mode-string (append global-mode-string '(enwc-display-string))))
  (unless enwc-display-mode-line-timer
    (setq enwc-display-mode-line-timer
          (run-at-time t 1 'enwc-update-mode-line)))
  (message "ENWC mode line enabled"))

(defun enwc-disable-display-mode-line ()
  "Disables the mode line display."
  (interactive)
  (or global-mode-string (setq global-mode-string '("")))
  (setq enwc-display-mode-line nil)
  (setq global-mode-string (remove 'enwc-display-string global-mode-string))
  (when enwc-display-mode-line-timer
    (cancel-timer enwc-display-mode-line-timer))
  (setq enwc-display-mode-line-timer nil)
  (message "ENWC mode line disabled"))

(defun enwc-toggle-display-mode-line ()
  "Toggles the mode line display."
  (interactive)
  (if (not enwc-display-mode-line)
      (enwc-enable-display-mode-line)
    (enwc-disable-display-mode-line)))

(defun enwc-toggle-auto-scan ()
  "Toggles automatic scanning.
This will use the current value of `enwc-auto-scan-interval'."
  (interactive)
  (let ((new (not enwc-auto-scan)))
    (if new
        (progn (setq enwc-scan-timer
                     (run-at-time t enwc-auto-scan-interval 'enwc-scan t))
               (message "Auto-scan enabled"))
      (cancel-timer enwc-scan-timer)
      (message "Auto scan disabled"))
    (setq enwc-auto-scan new)))

;;;;;;;;;;;;;;;;;;;
;; Scan internal ;;
;;;;;;;;;;;;;;;;;;;

(defun enwc-scan (&optional nodisp)
  "The frontend of the scanning routine.  Sets up and moves to
the ENWC buffer if necessary, and scans and displays the networks.
If NODISP is non-nil, then do not display the results in the ENWC
buffer."
  (interactive "p")
  (unless nodisp
    (setq enwc-scan-interactive t))
  (when (get-buffer "*ENWC*")
    (with-current-buffer "*ENWC*"
      (if enwc-using-wired
          (progn
            (enwc-scan-internal)
            (goto-char 0)
            (forward-line))
        (enwc-scan-internal)))))

(defun enwc-scan-internal-wireless ()
  "The initial scan routine.
This initiates a scan using D-Bus, then exits,
waiting for the callback.

All back-ends must call enwc-process-scan in some way
upon completion of a scan."
  (when enwc-scan-interactive
    (message "Scanning..."))
  (setq enwc-scan-requested t)
  (setq enwc-scan-done nil)
  (enwc-do-scan))

(defun enwc-scan-internal-wired ()
  "The scanning routine for a wired connection.
This gets the list of wired network profiles."
  (message "Updating Profiles...")
  (let ((profs (enwc-get-networks))
        fin-profs)
    (dolist (cur-prof profs)
      (when cur-prof
        (push cur-prof fin-profs)))
    (message "Updating Profiles... Done")
    (setq enwc-access-points fin-profs
          enwc-last-scan     fin-profs)
    fin-profs))

(defun enwc-scan-internal ()
  "The entry point for the internal scan routines.
This checks whether or not wired is being used,
 and runs the appropriate function."
  (if enwc-using-wired
      (enwc-scan-internal-wired)
    (enwc-scan-internal-wireless)))

(defun enwc-update-width (detail &optional val)
  "Update the width for column DETAIL to VAL.
This modifies the width entry in `enwc-details-alist' that corresponds to
DETAIL.

If VAL is not specified, then use the width of the display name for DETAIL."
  (let ((det (alist-get detail enwc-details-alist)))
    (unless val
      (setq val (1+ (length (alist-get 'display det)))))
    (setq val (max val (alist-get 'width det)))
    (setcdr (assq 'width det) val)))

(defun enwc-reset-widths ()
  "Reset the column widths for display."
  (dolist (det enwc-details-alist)
    (enwc-update-width (car det))))

(defun enwc-process-scan (&rest args)
  "The scanning callback.
After a scan has been performed, this processes and displays
the scan results."
  (unless (or enwc-using-wired (not enwc-scan-requested))
    (setq enwc-scan-requested nil
          enwc-access-points  (enwc-get-networks))
    (when enwc-scan-interactive
      (message "Scanning... Done"))
    (enwc-reset-widths)
    (setq enwc-last-scan (mapcar
                          (lambda (ap)
                            `(,ap . ,(enwc-get-wireless-nw-props ap)))
                          enwc-access-points))
    (dolist (nw enwc-last-scan)
      (dolist (props (cdr nw))
        (enwc-update-width (car props) (length (prin1-to-string (cdr props))))))
    (setq enwc-scan-done t)
    (enwc-display-wireless-networks enwc-last-scan)
    (setq enwc-scan-interactive nil)))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Networks ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-display-wired-networks (networks)
  "Displays the wired networks specified in the list NETWORKS.
NETWORKS must be in the form returned from
`enwc-scan-internal-wired'."
  (unless (listp networks)
    (error "NETWORKS must be a list of networks."))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Profile" 'face 'enwc-header-face))
    (insert "\n")
    (dolist (pr networks)
      (insert pr)
      (insert "\n"))))

(defmacro enwc--propertize-entry (network-entry)
  "Propertize network entry NETWORK-ENTRY."
  `(mapcar
    (lambda (det)
      (let* ((conv     (alist-get 'conv (cdr det)))
             (ent      (alist-get (car det) (cdr ,network-entry)))
             (conv-ent (funcall conv ent)))
        (if (equal cur-id (car nw))
            (propertize conv-ent
                        'font-lock-face
                        'enwc-connected-face)
          conv-ent)))
    enwc-details-alist))

(defun enwc-display-wireless-networks (networks)
  "Displays the networks in the list NETWORKS in the current buffer.
NETWORKS must be in the format returned by
`enwc-scan-internal-wireless'."
  (unless (get-buffer "*ENWC*")
    (enwc-setup-buffer t))
  (cl-check-type networks list)
  (with-current-buffer (get-buffer "*ENWC*")
    (let ((cur-id (enwc-get-current-nw-id)))
      (setq tabulated-list-format
            (apply 'vector
                   (mapcar
                    (lambda (det)
                      `(,(alist-get 'display (cdr det))
                        ,(alist-get 'width   (cdr det))))
                    enwc-details-alist)))

      (setq tabulated-list-entries
            (mapcar
             (lambda (nw)
               `(,(car nw)
                 ,(apply 'vector (enwc--propertize-entry nw))))
             networks)))
    (tabulated-list-init-header)

    (tabulated-list-print)))

(defun enwc-display-networks (networks)
  "Displays the network in NETWORKS.
This is an entry to the display functions,
and checks whether or not ENWC is using wired."
  (unless (eq major-mode 'enwc-mode)
    (enwc-setup-buffer))
  (cl-check-type networks list)
  (if enwc-using-wired
      (enwc-display-wired-networks networks)
    (enwc-display-wireless-networks networks)))

(defun enwc-find-network (essid &optional networks)
  "Checks through NETWORKS for the network with essid ESSID,
and returns the network identifier.  Uses `enwc-last-scan' if
NETWORKS is nil.  If the network is not found, then it returns nil.

   When called interactively, this only prints out what it finds.
Otherwise, it actually returns it."
  (interactive "sNetwork ESSID: ")
  (unless (or networks enwc-last-scan)
    (setq enwc-scan-interactive nil)
    (enwc-scan-internal))
  (let ((nets (or networks enwc-last-scan))
        need-break cur-net)
    (while (and nets (not cur-net))
      (setq cur-net (pop nets))
      (unless (string-equal (alist-get 'essid (cdr-safe cur-net))
                            essid)
        (setq cur-net nil)))
    (if cur-net
        (if (called-interactively-p 'any)
            (message "Network %d has essid %s" (number-to-string (car cur-net)) essid)
          (car cur-net))
      (when (called-interactively-p 'any)
        (message "Network not found.")))))

;;;;;;;;;;;;;;;;;;;;;
;; Connect Network ;;
;;;;;;;;;;;;;;;;;;;;;

(defun enwc-connect-network (id)
  "Connect to network with id ID.
This is an entry point for the internal connection functions,
and checks whether or not ENWC is using wired."
  (enwc-connect id)
  (if enwc-using-wired
      (nth id (enwc-get-networks))
    (when enwc-last-scan
      (enwc-value-from-scan 'essid id))))

(defun enwc-connect-to-network (net-id)
  "Connects the the network with network id NET-ID.
Confirms that NET-ID is a valid network id.
This calls `enwc-connect-network' as a subroutine."
  (interactive "sNetwork ID: ")
  (let (cur-net)
    (unless (enwc-is-valid-nw-id-p net-id)
      (error "Invalid network id."))
    (setq cur-net (enwc-connect-network net-id))
    (message (concat "Connecting to " cur-net))))

(defun enwc-connect-to-network-essid (essid)
  "Connects to the network with essid ESSID."
  (interactive "sNetwork ESSID: ")
  (let ((net-id (enwc-find-network essid)))
    (if net-id
        (enwc-connect-to-network net-id)
      (message "Network not found."))))

(defun enwc-connect-to-network-at-point ()
  "Connects to the network at the current line number.
Moves to the enwc buffer if necessary."
  (interactive)
  (unless (eq major-mode 'enwc-mode)
    (enwc-setup-buffer))
  ;;TODO: Fix this for wired (which doesn't have tabulated list)
  (enwc-connect-to-network (tabulated-list-get-id)))

(defun enwc-disconnect-network ()
  "Disconnects from the network, if any."
  (interactive)
  (message "Disconnecting")
  (enwc-disconnect))

(defun enwc-toggle-wired ()
  "Toggle the display and mode between wireless and wired.
This function also sets the variable `enwc-using-wired'."
  (interactive)
  (unless (eq major-mode 'enwc-mode)
    (enwc-setup-buffer))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq enwc-using-wired (not enwc-using-wired))
    (enwc-scan)))

(defun enwc ()
  "The main front-end to ENWC.
This sets up the buffer and scans for networks.
In order to use this, one must have already run
`enwc-setup'.

\\{enwc-mode-map}"
  (interactive)
  (enwc-setup-buffer)
  (enwc-scan))

;; Settings for access point AP
;;
;; IPv4 Settings:
;;   Address =
;;   Netmask =
;;   Gateway =
;;
;;   DNS 1   =
;;   DNS 2   =
;;
;; Security:
;;   Type    =
;;

(defun enwc-edit-view-entry ()
  "View the text of the entry at point.
This is mostly useful to view the text of the hidden entries."
  (interactive)
  (unless (get-buffer "*ENWC Edit*")
    (error "Not editing a network entry."))
  (unless (eq (current-buffer) (get-buffer "*ENWC Edit*"))
    (switch-to-buffer "*ENWC Edit*"))
  (unless (widget-at)
    (error "No widget at point"))
  (message (widget-field-value-get (widget-at))))

(define-widget 'enwc-profile-props-widget 'group
  "ENWC edit widget."
  :convert-widget 'identity
  :format "IPv4 Settings:\n %v"
  :value-to-internal 'enwc-profile-props-to-widget
  :value-to-external 'enwc-widget-to-profile-props
  :match #'(lambda nil t)
  :indent 1
  :args '((string :tag "Address")
          (string :tag "Netmask")
          (string :tag "Gateway")
          (string :tag "DNS1")
          (string :tag "DNS2")))

(defun enwc-profile-props-to-widget (widget props)
  "Create a profile props widget."
  (list
   (alist-get 'addr props "")
   (alist-get 'netmask props "")
   (alist-get 'gateway props "")
   (alist-get 'dns1 props "")
   (alist-get 'dns2 props "")))

(defun enwc-widget-to-profile-props (widget vals)
  (let ((addr (nth 0 vals))
        (netmask (nth 1 vals))
        (gateway (nth 2 vals))
        (dns1 (nth 3 vals))
        (dns2 (nth 4 vals)))
    `((addr . ,addr)
      (netmask . ,netmask)
      (gateway . ,gateway)
      (dns1 . ,dns1)
      (dns2 . ,dns2))))

;;;;;;;;;;;;;;
;; Security ;;
;;;;;;;;;;;;;;

(defmacro enwc--make-supplicant-multi (key &rest args)
  `(cons (quote ,key)
         (quote (checklist :tag ,(capitalize (enwc--sym-to-str key))
                           :format "%{%t%}: %v"
                           :sample-face bold
                           :indent ,(+ 4 (length (enwc--sym-to-str key)))
                           :args ,(mapcar
                                   (lambda (arg)
                                     `(item :tag ,arg :value ,(downcase arg)))
                                   args)))))

(defmacro enwc--make-supplicant-choice (key &rest args)
  `(cons (quote ,key)
         (quote (menu-choice :tag ,(capitalize (enwc--sym-to-str key))
                                     :format "%[%t%]: %v"
                                     :sample-face bold
                                     :args
                                     ,(mapcar
                                       (lambda (arg)
                                         `(item :tag ,(downcase arg) :value ,arg))
                                       args)))))

(defmacro enwc--make-supplicant-secret (key)
  `(cons (quote ,key)
         (quote (editable-field :tag ,(capitalize (enwc--sym-to-str key))
                                :format "%{%t%}: %v"
                                :sample-face bold
                                :keymap enwc-edit-field-map
                                :secret ?*))))

(defmacro enwc--make-supplicant-entry (key)
  `(cons (quote ,key)
         (quote (editable-field :tag ,(capitalize (enwc--sym-to-str key))
                                :sample-face bold
                                :format "%{%t%}: %v"))))

(defmacro enwc--make-supplicant-file (key)
  `(cons (quote ,key)
         (quote (file :tag ,(capitalize (enwc--sym-to-str key))
                      :format "%{%t%}: %v"
                      :sample-face bold
                      :must-match t))))

(defmacro enwc--make-supplicant-list (key &rest args)
  `(cons (quote ,key)
         (quote (list :tag ,(capitalize (enwc--sym-to-str key))
                      :format "%{%t%}: %v"
                      :sample-face bold
                      :indent ,(length (enwc--sym-to-str key))
                      :args ,(mapcar (lambda (x) (cdr (eval x))) args)))))

(defconst enwc-supplicant-alist
  (list
   (enwc--make-supplicant-multi proto "WPA" "RSN")
   (enwc--make-supplicant-multi key-mgmt "None" "WPA-PSK" "WPA-EAP" "IEEE8021X")
   (enwc--make-supplicant-choice auth-alg "OPEN" "SHARED" "LEAP")
   (enwc--make-supplicant-multi pairwise "CCMP" "TKIP" "NONE")
   (enwc--make-supplicant-multi group "CCMP" "TKIP" "WEP104" "WEP40")
   (enwc--make-supplicant-secret psk)
   (enwc--make-supplicant-secret wep-key0)
   (enwc--make-supplicant-secret wep-key1)
   (enwc--make-supplicant-secret wep-key2)
   (enwc--make-supplicant-secret wep-key3)
   (enwc--make-supplicant-choice wep-tx-keyidx "0" "1" "2" "3")
   (enwc--make-supplicant-choice eap "TLS" "PEAP" "TTLS" "LEAP" "FAST")
   (enwc--make-supplicant-entry identity)
   (enwc--make-supplicant-entry anonymous-identity)
   (enwc--make-supplicant-secret password)
   (enwc--make-supplicant-file ca-cert)
   (enwc--make-supplicant-file client-cert)
   (enwc--make-supplicant-file private-key)
   (enwc--make-supplicant-secret private-key-passwd)
   (enwc--make-supplicant-file pac-file)
   (enwc--make-supplicant-list phase1
                               (enwc--make-supplicant-choice peapver "" "0" "1")
                               (enwc--make-supplicant-choice peaplabel "" "0" "1")
                               (enwc--make-supplicant-choice fast-provisioning "" "0" "1" "2" "3"))
   (enwc--make-supplicant-list phase2
                               (enwc--make-supplicant-choice auth "" "MD5" "MSCHAPV2" "OTP" "GTC" "TLS")
                               (enwc--make-supplicant-choice autheap "" "MD5" "MSCHAPV2" "OTP" "GTC" "TLS")
                               (enwc--make-supplicant-file ca-cert)
                               (enwc--make-supplicant-file client-cert)
                               (enwc--make-supplicant-file private-key)
                               (enwc--make-supplicant-secret private-key-passwd)))
  "An alist that maps supplicant entries to a widget type.

For more information, see the documentation for wpa_supplicant.")

(defcustom enwc-supplicant-template-alist
  `((wep . ((key-mgmt . ("none"))
            (wep-key0 . req)
            (wep-tx-keyidx . "0")))
    (wpa2 . ((proto . ("WPA" "RSN"))
             (key-mgmt . "WPA-PSK")
             (pairwise . ("CCMP" "TKIP"))
             (group . ("CCMP" "TKIP"))
             (psk . req)))
    (leap . ((eap . "LEAP")
             (key-mgmt . ("IEEE8021X"))
             (auth-alg . "LEAP")
             (identity . req)
             (password . req)))
    (eap-fast . ((proto . ("RSN" "WPA"))
                 (pairwise . ("CCMP" "TKIP"))
                 (group . ("CCMP" "TKIP"))
                 (key-mgmt . ("WPA-EAP"))
                 (eap . "FAST")
                 (identity . req)
                 (password . req)
                 (phase1 . ((fast-provisioning . "1")))
                 (pac-file . opt)))
    (eap-tls . ((key-mgmt . ("WPA-EAP"))
                (pairwise . ("TKIP"))
                (group . ("TKIP"))
                (eap . "TLS")
                (identity . req)
                (ca-cert . opt)
                (client-cert . opt)
                (private-key . req)
                (private-key-passwd . req)))
    (peap . ((proto . ("RSN"))
             (key-mgmt . ("WPA-EAP"))
             (pairwise . ("CCMP"))
             (eap . "PEAP")
             (identity . req)
             (password . req)))
    (peap-tkip . ((proto . ("WPA"))
                  (key-mgmt . ("WPA-EAP"))
                  (pairwise . ("TKIP"))
                  (group . ("TKIP"))
                  (eap . "PEAP")
                  (identity . req)
                  (password . req)
                  (ca-cert . opt)
                  (phase1 . ((peaplabel . "0")))
                  (phase2 . ((auth . "MSCHAPV2"))))))
  "The alist of templates for security.
This should be an alist of the form (KEY . ((SUPPLICANT-KEY . INITIAL-INPUT) ...))
Each SUPPLICANT-KEY should be a key from `enwc-supplicant-alist', and INITIAL-INPUT
should be an acceptable value for SUPPLICANT-KEY.

If INITIAL-INPUT is the symbol req, then this option is required.
The value opt means that the option is optional."
  :group 'enwc
  :type '(alist :key-type symbol :value-type (alist :key-type symbol)))

(defun enwc--get-supplicant-entry (ent &optional sec-info)
  "Create a widget definition from ENT.

If optional parameter SEC-INFO is non-nil, then use it
for security information."
  (let ((init (cdr ent))
        (wid (assq (car ent) enwc-supplicant-alist))
        fin)
    (unless wid
      (error "Unknown supplicant type %s" (car ent)))
    ;; Set the initial value for the widget.
    (when (eq (cadr wid) 'list)
      (let (act-init)
        (dolist (arg (widget-get (cdr wid) :args))
          (push (alist-get (enwc--str-to-sym (downcase (widget-get arg :tag))) init "")
                act-init))
        (setq init (nreverse act-init))
        (print (car ent))
        (pp init)))
    (cons (cadr wid)
          (append (pcase init
                  (`req `(:required t :value ,(alist-get (car ent) sec-info "")))
                  (`opt `(:value ,(alist-get (car ent) sec-info "")))
                  (_ `(:value ,init)))
                (cddr wid)))))

(defun enwc-create-template-menu (&optional sec-info)
  "Create the widget declaration for the menu of templates.

If specified, SEC-INFO is passed to the templates to initialize them."
  `(menu-choice
    :tag "Security"
    :indent 2
    ,@(mapcar
       (lambda (tm)
         `(list
           :tag ,(symbol-name (car tm))
           :menu-tag ,(symbol-name (car tm))
           ,@(mapcar
              (lambda (ent)
                (enwc--get-supplicant-entry ent sec-info))
              (cdr tm))))
       enwc-supplicant-template-alist)))

(defun enwc-display-sec-widget (&optional sec-info)
  "Create the menu of security templates.
If specified, SEC-INFO is passed to the templates to initialize them."
  (widget-create (enwc-create-template-menu sec-info)))

(defun enwc-sec-widget-data (widget)
  "Get the data from a security widget WIDGET."
  (let* ((type (widget-get (widget-get widget :choice) :tag))
         (values (widget-value widget))
         (template (assq (intern type) enwc-supplicant-template-alist)))
    (unless template
      (error "Unrecognized security template."))
    (setq template (cdr template))
    (cons
     `(sec-type ,(intern type))
     (cl-mapcar
      (lambda (val v)
        (let ((vl v))
          (when (or (eq (car val) 'phase1)
                    (eq (car val) 'phase2))
            (let ((subs
                   (mapcar
                    (lambda (arg)
                      (enwc--str-to-sym (downcase (widget-get arg :tag))))
                    (widget-get (alist-get (car val) enwc-supplicant-alist) :args))))
              (setq vl (cl-mapcar 'cons subs v))))
          (cons (car val) vl)))
      template values))))

(defvar enwc-network-edit-widget nil
  "The network information widget used in the edit buffer.")

(defvar enwc-security-edit-widget nil
  "The security widget used in the edit buffer.")

(defun enwc-setup-edit-buffer ()
  "Setup the edit buffer.  This removes the old one if neccessary,
and redisplays the settings from the network profile
 with id `enwc-edit-id', which is set in `enwc-edit-entry-at-point'."
  (when (get-buffer "*ENWC Edit*")
    (kill-buffer "*ENWC Edit*"))
  (with-current-buffer (get-buffer-create "*ENWC Edit*")
    (let ((nw-info (enwc-get-profile-info enwc-edit-id)))

      (widget-insert (concat "Settings for access point "
                             (enwc-value-from-scan 'essid enwc-edit-id)
                             "\n"))
      (widget-insert "\n")
      (setq enwc-network-edit-widget
            (widget-create 'enwc-profile-props-widget :value nw-info))

      (widget-insert "\n")
      (setq enwc-security-edit-widget (enwc-display-sec-widget nw-info))

      (use-local-map enwc-edit-map)
      (widget-setup)))

  (switch-to-buffer "*ENWC Edit*"))

(defun enwc-edit-save ()
  "Save the network settings from the edit buffer."
  (interactive)
  (unless (get-buffer "*ENWC Edit*")
    (error "Not editing a network entry"))
  (unless (eq (current-buffer) (get-buffer "*ENWC Edit*"))
    (switch-to-buffer "*ENWC Edit*"))

  (enwc-save-nw-settings
   enwc-edit-id
   (append (widget-value enwc-network-edit-widget)
           (enwc-sec-widget-data enwc-security-edit-widget))))

(defun enwc-edit-entry-at-point ()
  "Edit the current network entry."
  (interactive)
  (setq enwc-edit-id (tabulated-list-get-id))
  (select-window (split-window))
  (enwc-setup-edit-buffer))

(defvar enwc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'enwc-scan)
    (define-key map (kbd "C") 'enwc-connect-to-network-essid)
    (define-key map (kbd "D") 'enwc-disconnect-network)
    (define-key map (kbd "W") 'enwc-toggle-wired)
    (define-key map (kbd "E") 'enwc-edit-entry-at-point)
    (define-key map (kbd "RET") 'enwc-connect-to-network-at-point)
    map)
  "The keymap for network display in ENWC.")

(defvar enwc-edit-map
  (let ((map (copy-keymap widget-keymap)))
    (define-key map (kbd "C-x C-s") 'enwc-edit-save)
    map)
  "The keymap for editing network profiles with ENWC.")

(defvar enwc-edit-field-map
  (let ((map (copy-keymap widget-field-keymap)))
    (define-key map (kbd "C-x C-a") 'enwc-edit-view-entry)
    map)
  "The keymap for editable fields within the ENWC edit buffer.")

(define-derived-mode enwc-mode tabulated-list-mode "enwc"
  "Mode for working with network connections.
\\{enwc-mode-map}"
  (add-hook 'tabulated-list-revert-hook 'enwc-scan nil t))

(defun enwc-setup-buffer (&optional nomove)
  "Sets up the ENWC buffer.
This first checks to see that it exists,
and if it doesn't, then create it.

If NOMOVE is non-nil, then do not move to the
newly created buffer."
  (unless (get-buffer "*ENWC*")
    (with-current-buffer (get-buffer-create "*ENWC*")
      (enwc-mode)))
  (unless nomove
    (switch-to-buffer "*ENWC*")))

(provide 'enwc)

;;; enwc.el ends here
