;;; enwc.el --- The Emacs Network Client

;; Copyright (C) 2012-2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: external, network, wicd, manager, nm
;; Version: 2.0beta1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://savannah.nongnu.org/p/enwc

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:
;;
;; ENWC is the Emacs Network Client.  It is designed to provide a front-end to
;; various network managers, such as NetworkManager and Wicd.
;;
;; Currently, only NetworkManager and Wicd are supported, although experimental
;; support exists for Connman.
;;
;; In order to use this package, add
;;
;; (setq enwc-default-backend BACKEND-SYMBOL)
;;
;; where BACKEND-SYMBOL is either 'wicd or 'nm, to your .emacs file (or other init
;; file).
;;
;; Then you can just run `enwc' to start everything.
;;
;; Example:
;;
;; (setq enwc-default-backend 'nm)

;;; TODO:
;;
;; - Add hooks for scan completion, and possibly upon network connection.
;; - Wired uses profiles, not networks; Refer to them as such
;; - Is an association list the best idea for scan results?  Perhaps a structure
;;   would work better?

;;; Code:

(require 'enwc-backend)

(require 'tabulated-list)
(require 'cl-lib)
(require 'cl-macs)
(require 'format-spec)

(require 'map)
(require 'seq)

(defgroup enwc nil
  "*The Emacs Network Client"
  :prefix "enwc-"
  :group 'external)

(defcustom enwc-wireless-device ""
  "The wireless device to use for ENWC."
  :group 'enwc
  :type 'string)

(defcustom enwc-wired-device ""
  "The wired device to use for ENWC."
  :group 'enwc
  :type 'string)

(defcustom enwc-display-mode-line t
  "Non-nil means display network information in the mode line.
The specific information can be set using `enwc-mode-line-format'."
  :group 'enwc
  :type 'boolean)

(defcustom enwc-auto-scan nil
  "Whether or not to have ENWC automatically scan.
If non-nil, then ENWC will automatically scan for
networks every `enwc-auto-scan-interval' seconds."
  :group 'enwc
  :type 'boolean)

(defcustom enwc-auto-scan-interval 20
  "The interval between automatic scans."
  :group 'enwc
  :type 'integer)

(defcustom enwc-mode-line-format " [%s%%] "
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

(defvar enwc-display-string " [0%] "
  "The mode line display string.
This is altered every second to display the current network strength
in `enwc-update-mode-line'.")

(defun enwc--print-strength (s)
  "Convert signal strength S to a string to dispay."
  (format "%s%%" s))

(defmacro enwc--make-number-sorter (n)
  `(lambda (a b)
     (let ((act-a (aref (nth 1 a) ,n))
           (act-b (aref (nth 1 b) ,n)))
       (< act-a act-b))))

(defalias 'enwc--str-sorter (enwc--make-number-sorter 0))
(defalias 'enwc--chnl-sorter (enwc--make-number-sorter 4))

(cl-defstruct enwc-column-spec ()
  detail display sorter width conv)

(defconst enwc-column-specs
  (list
   (make-enwc-column-spec
    :detail 'strength
    :display "Str"
    :sorter #'enwc--str-sorter
    :conv #'enwc--print-strength)
   (make-enwc-column-spec
    :detail 'essid
    :display "Essid"
    :sorter t
    :conv #'identity)
   (make-enwc-column-spec
    :detail 'encrypt
    :display "Encrypt"
    :sorter t
    :conv #'identity)
   (make-enwc-column-spec
    :detail 'bssid
    :display "Bssid"
    :sorter t
    :conv #'identity)
   (make-enwc-column-spec
    :detail 'channel
    :display "Channel"
    :sorter #'enwc--chnl-sorter
    :conv #'number-to-string)))

(defvar enwc-last-scan (make-hash-table :test #'equal)
  "The most recent scan results.

This will be an association list of the form:

((ID . ((strength . STRENGTH) (essid . ESSID) ...)) ...)

Each ID is a backend-specific network ID.

Each key in the children association lists corresponds to an entry in
`enwc-column-specs'.")

(defvar enwc-access-points nil
  "The most recent access point list.")

(defvar enwc-using-wired nil
  "Non-nil means ENWC is using wired connections.

Note that this is NOT the same as `enwc-is-wired'.  This checks
whether or not ENWC is in wired mode.")

(defvar enwc-edit-id nil
  "This is the network id of the network being edited.")

(defvar enwc-scan-requested nil
  "This is non-nil when a scan has been requested.
This is used so as to avoid multiple updates of the scan data.")

(defvar enwc-scan-interactive nil
  "This is non-nil that a scan was interactively requested.
This is only used internally.")

(defvar enwc-display-mode-line-timer nil
  "The timer that updates the mode line display.")

(defvar enwc-scan-timer nil
  "The timer for automatic scanning.")

(defvar enwc-mode-line-timer nil)

(make-local-variable 'enwc-edit-id)

;; The Faces

(defface enwc-header
  '((((class color) (background light))
     (:foreground "Blue"))
    (((class color) (background dark))
     (:foreground "Blue"))
    (t (:background "Blue")))
  "The face for the headers."
  :group 'enwc)

(defface enwc-connected
  '((((class color) (background dark))
     (:foreground "Green"))
    (((class color) (background light))
     (:foreground "Green"))
    (t (:background "Green")))
  "The face for the connected network."
  :group 'enwc)

;;;;;;;;;;;;;;;;;;;;
;; ENWC functions ;;
;;;;;;;;;;;;;;;;;;;;

(defun enwc-get-networks ()
  "Get the identifiers for the access points
from a previous scan."
  (enwc--network-ids enwc--current-backend))

(defun enwc-request-scan ()
  "Request a backend scan."
  (setq enwc-scan-requested t)
  (enwc--scan enwc--current-backend))

(defun enwc-connect (id)
  "Connect to network with id ID.

ID is specific to the backend."
  (enwc--connect enwc--current-backend id enwc-using-wired))

(defun enwc-disconnect ()
  "Disconnect from the current network."
  (enwc--disconnect enwc--current-backend enwc-using-wired))

(defun enwc-get-current-nw-id ()
  "Get the id of the current network id,
or nil if there isn't one.

The returned id is specific to the backend."
  (enwc--current-nw-id enwc--current-backend enwc-using-wired))

(defun enwc-check-connecting-p ()
  "Check to see if there is a connection in progress.
Returns `non-nil' if there is one, nil otherwise."
  (enwc--is-connecting-p enwc--current-backend))

(defun enwc-get-wireless-nw-props (id)
  "Get the network properties of the wireless network with id ID.
This will return an associative list with the keys
corresponding to `enwc-column-specs'.

ID is specific to the backend."
  (enwc--wireless-nw-props enwc--current-backend id))

(defun enwc-is-wired-p ()
  "Check whether or not ENWC is connected to a wired network.
Note that this is NOT the same as `enwc-using-wired'.
This checks for an active wired connection."
  (enwc--is-wired-p enwc--current-backend))

;;;;;;;;;;;;;;;;;;;;;;
;; Actual Functions ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun enwc-connected-to-network-p ()
    (enwc-is-valid-nw-id-p (enwc-get-current-nw-id)))

(defun enwc-is-valid-nw-id-p (id)
  "Confirm that ID is a valid network id."
  (and id (not (eq id 'wired))))

(defun enwc-value-from-scan (detail &optional id)
  "Retrieve a value for DETAIL from `enwc-last-scan'.
If ID is specified, then it will get the entry for ID.
Otherwise, ID is set to the current network ID.

If DETAIL is not found in `enwc-last-scan', then return nil."
  (setq id (or id (enwc-get-current-nw-id)))
  (when enwc-last-scan
    (map-nested-elt enwc-last-scan `(,id ,detail))))

(defun enwc-make-format-spec ()
  "Create a format specification for the mode line string."
  ;; Variables here are cached to avoid latencies when communicating with D-Bus.
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
  "Format the mode line string.
This is derived from `enwc-mode-line-format'.
See the documentation for it for more details."
  (format-spec enwc-mode-line-format (enwc-make-format-spec)))

(defun enwc-update-mode-line ()
  "Update the mode line display.
This uses the format specified by `enwc-mode-line-format'.
This is initiated during setup, and runs once every second."
  (setq enwc-display-string (enwc-format-mode-line-string))
  (force-mode-line-update))

(defun enwc-enable-display-mode-line ()
  "Enable the mode line display."
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
  "Disable the mode line display."
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

(defun enwc-enable-auto-scan ()
  "Enable auto scanning."
  (interactive)
  (unless enwc-scan-timer
    (setq enwc-scan-timer
          (run-at-time t enwc-auto-scan-interval 'enwc-scan t)))
  (setq enwc-auto-scan t)
  (message "Auto-scan enabled"))

(defun enwc-disable-auto-scan ()
  "Disable auto scanning."
  (interactive)
  (when enwc-scan-timer (cancel-timer enwc-scan-timer))
  (setq enwc-auto-scan nil)
  (message "Auto scan disabled"))

(defun enwc-toggle-auto-scan ()
  "Toggles automatic scanning.
This will use the current value of `enwc-auto-scan-interval'."
  (interactive)
  (if enwc-auto-scan
      (enwc-disable-auto-scan)
    (enwc-enable-auto-scan)))

;;;;;;;;;;;;;;;;;;;
;; Scan internal ;;
;;;;;;;;;;;;;;;;;;;

(defun enwc-scan (&optional nodisp)
  "The frontend of the scanning routine.
Set up and move to the ENWC buffer if necessary, and scan and display the
 networks.  If NODISP is `non-nil', then do not display the results in the ENWC
buffer."
  (interactive)
  (unless nodisp
    (setq enwc-scan-interactive t)
    (enwc-ensure-buffer))
  (with-current-buffer "*ENWC*"
    (enwc-scan-internal)))

(defun enwc-scan-internal-wireless ()
  "The initial scan routine for wireless networks.
This initiates a scan using D-Bus, then exits, waiting for the callback.

All back-ends must call `enwc-process-scan' in some way upon completion of a
 scan."
  (when enwc-scan-interactive
    (message "Scanning..."))
  (enwc-request-scan))

(defun enwc-scan-internal-wired ()
  "The scanning routine for a wired connection.
This gets the list of wired network profiles."
  (message "Updating Profiles...")
  (let ((profs (enwc-get-networks)))
    (message "Updating Profiles... Done")
    (setq enwc-access-points profs
          enwc-last-scan     profs)
    (enwc-display-wired-networks profs)))

(defun enwc-scan-internal ()
  "The entry point for the internal scan routines.
This checks whether or not wired is being used, and runs the appropriate
 function."
  (if enwc-using-wired
      (enwc-scan-internal-wired)
    (enwc-scan-internal-wireless)))

(defun enwc--update-scan-results ()
  (setq enwc-last-scan (make-hash-table :test #'equal))
  (dolist (ap (enwc-get-networks))
    (puthash ap (enwc-get-wireless-nw-props ap) enwc-last-scan)))

(defun enwc-redisplay-wireless-networks ()
  (interactive)
  (enwc--update-scan-results)
  (enwc-display-wireless-networks enwc-last-scan))

(defun enwc-process-scan (&rest args)
  "The scanning callback.
After a scan has been performed, this processes and displays the scan results.

ARGS is only for compatibility with the calling function."
  (unless (or enwc-using-wired (not enwc-scan-requested))
    (setq enwc-scan-requested nil)
    (when enwc-scan-interactive
      (message "Scanning... Done"))
    (enwc--update-scan-results)
    (enwc-display-wireless-networks enwc-last-scan)
    (setq enwc-scan-interactive nil)))

;;;;;;;;;;;;;;;;;;;;;;
;; Display Networks ;;
;;;;;;;;;;;;;;;;;;;;;;

(cl-defun enwc-refresh-widths (&optional (networks enwc-last-scan))
  "Refresh the column widths for display."
  (setq enwc-column-specs
        (mapcar
         (lambda (spec)
           (pcase-let* (((cl-struct enwc-column-spec detail display conv) spec)
                        (new-max (seq-max
                                  (map-apply
                                   (lambda (id nw)
                                     (length (funcall conv (alist-get detail nw))))
                                   networks)))
                        (min-width (+ (length display) 2)))
             (setf (enwc-column-spec-width spec) (max new-max min-width)))
           spec)
         enwc-column-specs)))

(defun enwc-display-wired-networks (networks)
  "Display the wired networks specified in the list NETWORKS.
NETWORKS must be in the form returned from
`enwc-scan-internal-wired'."
  (let ((inhibit-read-only t))
    (setq tabulated-list-format (vector '("Profile" . 1)))
    ;;TODO: actually get names of profiles, if possible.
    (setq tabulated-list-entries (mapcar (lambda (prop) (cons prop prop)) networks))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun enwc--get-details (network-entry)
  (mapcar
   (lambda (detail)
     (alist-get (enwc-column-spec-detail detail) network-entry))
   enwc-column-specs))

(defun enwc--tabulated-list-entries ()
  (map-apply
   (lambda (id nw) (list id (vconcat (enwc--get-details nw))))
   enwc-last-scan))

(defun enwc--tabulated-list-printer (id cols)
  "Print the row ID with column values COL."
  (let ((cur-id (enwc-get-current-nw-id)))
    (tabulated-list-print-entry
     id
     (vconcat
      (seq-map-indexed
       (lambda (col idx)
         (let* ((detail (nth idx enwc-column-specs))
                (conv   (funcall (enwc-column-spec-conv detail) col)))
           (if (equal cur-id id)
               (propertize conv 'font-lock-face 'enwc-connected)
             conv)))
       cols)))))

(defun enwc-display-wireless-networks (networks)
  "Display the networks in the list NETWORKS in the current buffer."
  (enwc-ensure-buffer)
  ;; Update the display widths.
  (enwc-refresh-widths)
  (with-current-buffer (get-buffer "*ENWC*")
    (setq tabulated-list-format
          (vconcat
           (mapcar
            (pcase-lambda ((cl-struct enwc-column-spec display width sorter))
              (list display width sorter))
            enwc-column-specs)))
    (setq tabulated-list-entries #'enwc--tabulated-list-entries)
    (setq tabulated-list-printer #'enwc--tabulated-list-printer)

    (tabulated-list-init-header)

    (tabulated-list-print)))

(defun enwc-display-networks (networks)
  "Displays the network in NETWORKS.
This is an entry to the display functions, and checks whether or not ENWC is
 using wired."
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
      id
    (when enwc-last-scan
      (enwc-value-from-scan 'essid id))))

(defun enwc-connect-to-network (net-id)
  "Connect the the network with network id NET-ID.
Confirms that NET-ID is a valid network id.
This calls `enwc-connect-network' as a subroutine."
  (interactive "sNetwork ID: ")
  (let (cur-net)
    (unless (enwc-is-valid-nw-id-p net-id)
      (error "Invalid network id."))
    (setq cur-net (enwc-connect-network net-id))
    (message "Connecting to %s" cur-net)))

(defun enwc-connect-to-network-essid (essid)
  "Connect to the network with essid ESSID."
  (interactive "sNetwork ESSID: ")
  (let ((net-id (enwc-find-network essid)))
    (if net-id
        (enwc-connect-to-network net-id)
      (message "Network not found."))))

(defun enwc-connect-to-network-at-point ()
  "Connect to the network at the current line number.
Moves to the enwc buffer if necessary."
  (interactive)
  (unless (eq major-mode 'enwc-mode)
    (enwc-setup-buffer))
  ;;TODO: Fix this for wired (which doesn't have tabulated list)
  (enwc-connect-to-network (tabulated-list-get-id)))

(defun enwc-disconnect-network ()
  "Disconnect from the network, if any."
  (interactive)
  (message "Disconnecting")
  (enwc-disconnect))

(defun enwc-toggle-wired ()
  "Toggle the display and mode between wireless and wired.
This has the side-effect of setting the variable `enwc-using-wired', and calling
a scan."
  (interactive)
  (unless (eq major-mode 'enwc-mode)
    (enwc-setup-buffer))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq enwc-using-wired (not enwc-using-wired))
    (enwc-scan)))



(defcustom enwc-interface-list-function 'enwc--ip-interface-list
  "Function to use to collect network interfaces.

ENWC comes with two functions for this purpose:

  - enwc--ip-interface-list: Use ip to determine the network interfaces

  - enwc--ifconfig-interface-list: Use ifconfig to determine the network interfaces

This must be a function of no arguments that returns a list of
strings.  Each element of the returned list should be a network
interface, i.e. lo or eth0."
  :group 'enwc
  :type 'function)

(defun enwc--ip-interface-list ()
  "Use `ip' to get a list of network interfaces."
  (let ((interfaces))
    (with-temp-buffer
      (call-process "ip" nil t nil "link")
      (goto-char (point-min))
      (while (re-search-forward "^[0-9]+:\s-*\\([[:alnum:]]+\\):" nil t)
        (push (match-string 1) interfaces)))
    (nreverse interfaces)))

(defun enwc--ifconfig-interface-list ()
  "Use `ifconfig' to get a list of network interfaces."
  (let ((interfaces))
    (with-temp-buffer
      (call-process "ifconfig" nil t nil "-a")
      (goto-char (point-min))
      (while (re-search-forward "^\\([[:alnum:]]+\\):" nil t)
        (push (match-string 1) interfaces)))
    (nreverse interfaces)))

(defun enwc--select-interfaces ()
  "Collect a list of network interfaces and prompt the user to select two.

One interface will be used for wireless, and the other for wired.

There is no need to call this function manually; that should be
left to `enwc-setup'.  Instead, set `enwc-wireless-device' and
`enwc-wired-device'."
  (let ((interfaces (funcall enwc-interface-list-function)))
    (when (string-empty-p enwc-wired-device)
      (setq enwc-wired-device (completing-read "Wired Interface: " interfaces)))
    (when (string-empty-p enwc-wireless-device)
      (setq enwc-wireless-device (completing-read "Wireless Interface: " interfaces)))))

(defvar enwc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'enwc-scan)
    (define-key map (kbd "C") 'enwc-connect-to-network-essid)
    (define-key map (kbd "D") 'enwc-disconnect-network)
    (define-key map (kbd "W") 'enwc-toggle-wired)
    ;; (define-key map (kbd "E") 'enwc-edit-entry-at-point)
    (define-key map (kbd "RET") 'enwc-connect-to-network-at-point)
    map)
  "The keymap for network display in ENWC.")

(define-derived-mode enwc-mode tabulated-list-mode "enwc"
  "Mode for working with network connections.
\\{enwc-mode-map}"
  (add-hook 'tabulated-list-revert-hook 'enwc-redisplay-wireless-networks nil t))

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

(defun enwc-ensure-buffer ()
  (unless (get-buffer "*ENWC*")
    (enwc-setup-buffer t)))

(defvar enwc--setup-done nil
  "Non-nil if enwc has already been set up.")

(defun enwc-setup ()
  "Set up ENWC.

If `enwc-wired-device' or `enwc-wireless-device' is empty, prompt
the user to set them from a list of interfaces.

Load the default backend, forcing it if
`enwc-force-backend-loading' is non-nil.

If `enwc-display-mode-line' is non-nil, enable the mode line.

If `enwc-auto-scan' is non-nil, start the auto-scan timer."
  (unless enwc--setup-done
    (when (or (string-empty-p enwc-wired-device)
              (string-empty-p enwc-wireless-device))
      (enwc--select-interfaces))

    (enwc-load-default-backend enwc-force-backend-loading)

    (when enwc-display-mode-line
      (enwc-enable-display-mode-line))

    (when (and enwc-auto-scan
               (> enwc-auto-scan-interval 0)
               (not enwc-scan-timer))
      (setq enwc-scan-timer
            (run-at-time t enwc-auto-scan-interval 'enwc-scan t)))

    (setq enwc--setup-done t)))

;;;###autoload
(defun enwc ()
  "The main front-end to ENWC.
This sets up the buffer and scans for networks.
In order to use this, one must have already run
`enwc-setup'.

\\{enwc-mode-map}"
  (interactive)
  (enwc-setup)
  (enwc-setup-buffer)
  (enwc-scan))

(provide 'enwc)

;;; enwc.el ends here
