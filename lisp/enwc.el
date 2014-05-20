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

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup enwc nil
  "*The Emacs Network Client"
  :prefix "ewnc-"
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

%s = The current signal strength.  If wired, then this is set to
100.

%e = The essid of the current network.  If wired, then this set to
'Wired'

%b = The bssid of the current network.  If using a wired connection,
then this is set to 'Wired'.

%% = A Normal '%'"
  :group 'enwc
  :type 'string)

;;; The function variables for the abstract layer.

(defvar enwc-scan-func nil
  "The function variable for the scan function.
This variable is set during setup.")

(defvar enwc-get-nw-func nil
  "A function variable to be used in `enwc-get-nw'.
This is redefined during setup to be the function to get the network
 list.")

(defvar enwc-get-wireless-nw-prop-func nil
  "A function variable to be used in `enwc-get-wireless-nw-prop'.
This is redefined during setup to be the function to get
a wireless network property.")

(defvar enwc-get-wireless-nw-props-func nil)

(defvar enwc-get-encryption-type-func nil
  "A function variable to be used in `enwc-get-encryption-type'.
This is redefined during setup to be the function to get the encryption
type for the selected backend.")

(defvar enwc-wireless-connect-func nil
  "The function variable for the wireless connect function.
This is redefined during setup to be the function to connect
for the selected backend.")

(defvar enwc-get-current-nw-id-func nil
  "The function variable to be used in `enwc-get-current-nw-id'.
This is redefined during setup to be the function to get
the current network id.")

(defvar enwc-check-connecting-func nil
  "The function variable to be used in `enwc-check-connecting'.
This is redefined during setup to be the function to
check whether or not ENWC is connecting.")

(defvar enwc-wireless-disconnect-func nil
  "The function variable for the wireless disconnect function.
This is redefined during setup to be the function to
disconnect from the wireless network.")

(defvar enwc-get-wired-profiles-func nil
  "The function variable to be used in `enwc-get-wired-profiles'.
This is redefined during setup to be the function to
get the list of wired network profiles.")

(defvar enwc-wired-connect-func nil
  "The function variable for the wired connect function.
This is redefined during setup to be the function
to connect to a wired network.")

(defvar enwc-wired-disconnect-func nil
  "The function variable for the wired disconnect function.
This is redefined during setup to be the function
to disconnect from a wired network.")

(defvar enwc-is-wired-func nil
  "The function variable to be used in `enwc-is-wired'.
This is redefined during setup to be the function to
check whether or not a wired connection is active.")

(defvar enwc-get-wired-nw-prop-func nil
  "The function variable to be used in `enwc-get-wired-nw-prop'.
This is redefined during setup to be the function to get
a network property from a wired network.")

(defvar enwc-get-sec-types-func nil
  "The function variable to be used in `enwc-get-sec-types'.
This is redefined during setup to be the function to get
the security types for a given network.")

(defvar enwc-get-ip-addr-func nil
  "The function variable to be used in `enwc-get-ip-addr'.
This is redefined during setup to be the function to get
the IP Address of a given network.")

(defvar enwc-get-netmask-func nil
  "The function variable to be used in `enwc-get-netmask'.
This is redefined during setup to be the function to get
the Netmask of a given network.")

(defvar enwc-get-gateway-func nil
  "The function variable to be used in `enwc-get-gateway'.
This is redefined during setup to be the function to get
the Gateway of a given network.")

(defvar enwc-get-dns-func nil
  "The function variable to be used in `enwc-get-dns'.
This is redefined during setup to be the function to get
the DNS Server Addresses for a given network.")

(defvar enwc-get-nw-info-func nil)

(defvar enwc-save-nw-settings-func nil
  "The function variable to be used in `enwc-save-nw-settings'.
This is redefined during setup to be the function to save
the network settings of a given network.")

(defvar enwc-details-list
  '("essid" "bssid" "strength" "mode" "encrypt" "channel")
  "The network details list.

This is redefined during setup to be the details list
for the selected backend.

This usually includes signal strength, essid, encryption type,
bssid, mode, and channel.")

(defvar enwc-display-string " [0%] "
  "The mode line display string.
This is altered every second to display the current network strength
in `enwc-update-mode-line'.")

;; (setq tabulated-list-format (vector `("ID" ,enwc-id-width sort) ...))
;; (setq tabulated-list-entries `((,id [id str essid encrypt ...]) ...))
;; (tabulated-list-init-header)
;; (tabulated-list-print)

(defvar enwc-wireless-headers '("ID" "STR" "ESSID"
				"ENCRYPT" "BSSID" "MODE" "CHNL")
  "The list of headers to be displayed in the ENWC buffer.
These correspond to the details in `enwc-details-list'.")

(defvar enwc-id-width 3
  "The width of the id column.")
(defvar enwc-strength-width 5
  "The width of the strength column.")
(defvar enwc-essid-width 5
  "The initial width of the essid column.
This is reset in wicd-scan-internal.")
(defvar enwc-encrypt-width 10
  "The width of the encryption column.")
(defvar enwc-bssid-width 18
  "The width of the bssid column.")
(defvar enwc-mode-width 16
  "The width of the mode column.")
(defvar enwc-channel-width 3
  "The width of the channel column.")

(defvar enwc-last-scan nil
  "The most recent scan results.")

(defvar enwc-access-points nil
  "The most recent access point list.")

(defvar enwc-using-wired nil
  "Whether or not wired mode is active.

This is `non-NIL' if ENWC is using wired connections.
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

;;;;;;;;;;;;;;;;;;;;
;; ENWC functions
;;;;;;;;;;;;;;;;;;;;

(defun enwc-do-scan ()
  "Runs a backend scan."
  (funcall enwc-scan-func))

(defun enwc-get-nw ()
  "Gets the identifiers for the access points
from a previous scan."
  (funcall enwc-get-nw-func))

(defun enwc-get-current-nw-id ()
  "Gets the id of the current network id,
or `nil' if there isn't one."
  (funcall enwc-get-current-nw-id-func enwc-using-wired))

(defun enwc-check-connecting-p ()
  "Checks to see if there is a connection
in progress.  Returns `non-NIL' if there is one,
`NIL' otherwise."
  (funcall enwc-check-connecting-func))

(defun enwc-get-wireless-nw-prop (id prop)
  "Gets property PROP from wireless network with id
ID and returns it."
  (funcall enwc-get-wireless-nw-prop-func id prop))

(defun enwc-get-wireless-nw-props (id)
  (funcall enwc-get-wireless-nw-props-func id))

(defun enwc-get-encryption-type (id)
  "Gets the encryption type used by the wireless
network with id ID."
  (funcall enwc-get-encryption-type-func id))

(defun enwc-get-wired-profiles ()
  "Gets the list of wired profiles."
  (funcall enwc-get-wired-profiles-func))

;;TODO: Add hooks to run after connecting.
(defun enwc-wireless-connect (id)
  "Begins a connection to wireless network with
id ID."
  (funcall enwc-wireless-connect-func id))

(defun enwc-wireless-disconnect ()
  "Disconnects the wireless."
  (funcall enwc-wireless-disconnect-func))

(defun enwc-wired-connect (id)
  "Connects to the wired profile with id ID."
  (funcall enwc-wired-connect-func id))

(defun enwc-wired-disconnect ()
  "Disconnects from the current network."
  (funcall enwc-wired-disconnect-func))

(defun enwc-is-wired-p ()
  "Checks whether or not ENWC is connected to
a wired network.
Note that this is NOT the same as `enwc-using-wired'.
This checks for an active wired connection."
  (funcall enwc-is-wired-func))

(defun enwc-get-sec-types (wired)
  "Gets the security types for network.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-get-sec-types-func wired))

(defun enwc-get-network-ent (wired id ent)
  "Gets network entry ENT from the network with network id ID.
WIRED is set to indicate whether or not this is
a wired network."
  (if wired
      nil
    (enwc-get-wireless-nw-prop id ent)))

(defun enwc-get-wired-nw-prop (id prop)
  "Gets network property PROP from
 the wired network with network id ID."
  (funcall enwc-get-wired-nw-prop-func id prop))

(defun enwc-get-ip-addr (wired id)
  "Gets the IP Address from the network with network id ID.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-get-ip-addr-func wired id))

(defun enwc-get-netmask (wired id)
  "Gets the Netmask from the network with network id ID.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-get-netmask-func wired id))

(defun enwc-get-gateway (wired id)
  "Gets the Gateway from the network with network id ID.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-get-gateway-func wired id))

(defun enwc-get-dns (wired id)
  "Gets the DNS Servers from the network with network id ID.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-get-dns-func wired id))

(defun enwc-get-nw-info (wired id)
  (funcall enwc-get-nw-info-func wired id))

(defun enwc-save-nw-settings (wired id settings)
  "Saves network settings SETTINGS to the network profile with
network id ID.
SETTINGS is an association list with entries for the IP Address,
Netmask, Gateway, DNS Servers, and Security.
WIRED is set to indicate whether or not this is
a wired network."
  (funcall enwc-save-nw-settings-func wired id settings))

;;;;;;;;;;;;;;;;;;;;;
;; Actual Functions
;;;;;;;;;;;;;;;;;;;;;

(defun enwc-is-valid-nw-id (id)
  "Confirms that ID is a valid network id."
  (<= 0 id))

(defun enwc-get-nw-prop (wired id prop)
  "Small function to get network property PROP from the network
with network id ID.
WIRED indicates whether or not this is a wired connection."
  (if wired
      (enwc-get-wired-nw-prop id prop)
    (enwc-get-wireless-nw-prop id prop)))

(defun enwc-format-mode-line-string ()
  "Formats the mode line string.
This is derived from `enwc-mode-line-format'.
See the documentation for it for more details."
  (let* ((f enwc-mode-line-format)
         (p 0)
         (l (length f))
         (cur-id (enwc-get-current-nw-id))
         (wiredp (enwc-is-wired-p))
         (connectingp (enwc-check-connecting-p))
         c fin-str)
    (while (< p l)
      (setq c (elt f p)
            p (1+ p))
      (setq fin-str
            (concat
             fin-str
             (if (not (eq c ?%))
                 (char-to-string c)
               (setq p (1+ p))
               (cond
                ((eq (elt f (1- p)) ?s)
                 (cond
                  (wiredp "100")
                  ((or (not (enwc-is-valid-nw-id cur-id))
                       (not enwc-last-scan))"0")
                  (connectingp "*")
                  (t (number-to-string
                      (cdr (assoc "strength" (nth cur-id enwc-last-scan)))))))
                ((eq (elt f (1- p)) ?e)
                 (cond
                  (wiredp "Wired")
                  ((or (not (enwc-is-valid-nw-id cur-id))
                       connectingp
                       (not enwc-last-scan)) "None")
                  (t (cdr (assoc "essid" (nth cur-id enwc-last-scan))))))
                ((eq (elt f (1- p)) ?b)
                 (cond
                  (wiredp "wired")
                  ((or (not (enwc-is-valid-nw-id cur-id))
                       connectingp
                       (not enwc-last-scan)) "none")
                  (t (cdr (assoc "bssid" (nth cur-id enwc-last-scan))))))
                ((eq (elt f (1- p)) ?%) "%"))))))
    fin-str))

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
  (unless (member 'enwc-display-string
                  global-mode-string)
    (setq global-mode-string (append global-mode-string '(enwc-display-string))))
  (if (not enwc-display-mode-line-timer)
      (setq enwc-display-mode-line-timer
            (run-at-time t 1 'enwc-update-mode-line)))
  (message "ENWC mode line enabled"))

(defun enwc-disable-display-mode-line ()
  "Disables the mode line display."
  (interactive)
  (or global-mode-string (setq global-mode-string '("")))
  (setq enwc-display-mode-line nil)
  (setq global-mode-string (remove 'enwc-display-string global-mode-string))
  (if enwc-display-mode-line-timer
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

;;;;;;;;;;;;;;;;;;
;; Scan internal
;;;;;;;;;;;;;;;;;;

(defun enwc-scan-internal-wireless ()
  "The initial scan routine.
This initiates a scan using D-Bus, then exits,
waiting for the callback.

All back-ends must call enwc-process-scan in some way
upon completion of a scan."
  (if enwc-scan-interactive
      (message "Scanning..."))
  (setq enwc-scan-requested t)
  (setq enwc-scan-done nil)
  (enwc-do-scan))

(defun enwc-process-scan (&rest args)
  "The scanning callback.
After a scan has been performed, this processes and displays
the scan results."
  (if (or enwc-using-wired (not enwc-scan-requested))
      nil
    (setq enwc-scan-requested nil)
    (let ((cur-id 0)
	  (nw-prop-list nil))
      (if enwc-scan-interactive
          (message "Scanning... Done"))
      (setq enwc-access-points (enwc-get-nw))
      (dolist (det enwc-details-list)
        (set (intern (concat "enwc-" det "-width"))
             1))
      (setq nw-prop-list
	    (mapcar 'enwc-get-wireless-nw-props
		    (number-sequence 0 (1- (length enwc-access-points)))))
      (setq enwc-last-scan
	    (mapcar (lambda (x)
		      (let ((ret-itm (cons (cons "id" cur-id) nil))
			    (prop-list (pop nw-prop-list)))
			(setq cur-id (1+ cur-id))
			(dolist (det enwc-details-list)
			  (let* ((cur-item (cdr (assoc det prop-list)))
                                 (ident det)
                                 (width-name (concat "enwc-"
                                                     ident
                                                     "-width"))
                                 (cur-width (eval (intern width-name)))
                                 (pos-len (if (and cur-item
                                                   (not (equal ident "strength")))
                                              (length cur-item)
                                            0)))
                            (set (intern width-name)
                                  (max cur-width pos-len))
			    (setq ret-itm (append ret-itm
						  (cons (cons ident
							      cur-item)
							nil)))))
			ret-itm))
		    (number-sequence 0 (1- (length enwc-access-points))))))
    (setq enwc-essid-width (1+ enwc-essid-width))
    (setq enwc-scan-done t)
    ;;(if enwc-scan-interactive
    (enwc-display-wireless-networks enwc-last-scan);;)
    (setq enwc-scan-interactive nil)))

(defun enwc-scan-internal-wired ()
  "The scanning routine for a wired connection.
This gets the list of wired network profiles."
  (message "Updating Profiles...")
  (let ((profs (enwc-get-wired-profiles))
	cur-prof fin-profs)
    (while profs
      (setq cur-prof (pop profs))
      (if cur-prof
	  (setq fin-profs (cons cur-prof
				fin-profs))))
    (message "Updating Profiles... Done")
    (setq enwc-access-points fin-profs)
    (setq enwc-last-scan fin-profs)
    fin-profs))

(defun enwc-scan-internal ()
  "The entry point for the internal scan routines.
This checks whether or not wired is being used,
 and runs the appropriate function."
  (if enwc-using-wired
      (enwc-scan-internal-wired)
    (enwc-scan-internal-wireless)))

;;;;;;;;;;;;;;;;;;;;;
;; Display Networks
;;;;;;;;;;;;;;;;;;;;;

(defun enwc-display-wired-networks (networks)
  "Displays the wired networks specified in the list NETWORKS.
NETWORKS must be in the form returned from
`enwc-scan-internal-wired'."
  (if (not (listp networks))
      (error "NETWORKS must be a list of networks."))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Profile" 'face 'enwc-header-face))
    (insert "\n")
    (dolist (pr networks)
      (insert pr)
      (insert "\n"))))

(defun enwc-insert-ent (ent width)
  "Small function to insert a network property entry.
ENT is the entry, and WIDTH is the column width."
  (insert ent)
  (insert-char 32 (- width (length ent))))

(defmacro enwc-maybe-pretty-entry (entry)
  `(if (eq cur-id (cdr (assoc "id" nw)))
       (propertize ,entry
                   'font-lock-face 'enwc-connected-face)
     ,entry))

(defun enwc-display-wireless-networks (networks)
  "Displays the networks in the list NETWORKS in the current buffer.
NETWORKS must be in the format returned by
`enwc-scan-internal-wireless'."
  (if (not (get-buffer "*ENWC*"))
      (enwc-setup-buffer t))
  (if (not (listp networks))
      (error "NETWORKS must be a list of association lists."))
  (with-current-buffer (get-buffer "*ENWC*")
    (let ((cur-id (enwc-get-current-nw-id))
          entries)
      (let ((header enwc-wireless-headers)
            (pos 0))

        (setq tabulated-list-format
              (vector `("ID" ,enwc-id-width)
                      '("STR" 4)
                      `("ESSID" ,enwc-essid-width)
                      `("ENCRYPT" ,enwc-encrypt-width)
                      `("BSSID" ,enwc-bssid-width)
                      `("MODE" ,enwc-mode-width)
                      `("CHNL" ,enwc-channel-width))))

      (dolist (nw networks)
        (let ((id (cdr (assoc "id" nw)))
              entry)
          (setq entry (list nil
                            (vector
                             (enwc-maybe-pretty-entry (number-to-string (cdr (assoc "id" nw))))
                             (enwc-maybe-pretty-entry
                              (concat (number-to-string (cdr (assoc "strength" nw)))
                                      "%"))
                             (enwc-maybe-pretty-entry (cdr (assoc "essid" nw)))
                             (enwc-maybe-pretty-entry (cdr (assoc "encrypt" nw)))
                             (enwc-maybe-pretty-entry (cdr (assoc "bssid" nw)))
                             (enwc-maybe-pretty-entry (cdr (assoc "mode" nw)))
                             (enwc-maybe-pretty-entry (cdr (assoc "channel" nw))))))
          (setq entries (cons entry entries))))

      (setq tabulated-list-entries (nreverse entries))
      (tabulated-list-init-header)

      (tabulated-list-print))))

(defun enwc-display-networks (networks)
  "Displays the network in NETWORKS.
This is an entry to the display functions,
and checks whether or not ENWC is using wired."
  (if (not (eq major-mode 'enwc-mode))
      (enwc-setup-buffer))
  (if (not (listp networks))
      (error "NETWORKS must be a list."))
  (if enwc-using-wired
      (enwc-display-wired-networks networks)
    (enwc-display-wireless-networks networks)))

(defun enwc-scan (&optional nodisp)
  "The frontend of the scanning routine.  Sets up and moves to
the ENWC buffer if necessary, and scans and displays the networks.
If NODISP is non-nil, then do not display the results in the ENWC
buffer."
  (interactive "p")
  (if (not nodisp)
      (setq enwc-scan-interactive t))
  (if (get-buffer "*ENWC*")
      (with-current-buffer "*ENWC*"
	(if enwc-using-wired
	    (progn
	      (enwc-scan-internal)
	      (goto-char 0)
	      (forward-line))
	  (enwc-scan-internal)))))
  
(defun enwc-find-network (essid &optional networks)
  "Checks through NETWORKS for the network with essid ESSID,
and returns the network identifier.  Uses `enwc-last-scan' if
NETWORKS is nil.  If the network is not found, then it returns nil.

   When called interactively, this only prints out what it finds.
Otherwise, it actually returns it."
  (interactive "sNetwork ESSID: ")
  (if (not (or networks enwc-last-scan))
      (progn
	(setq enwc-scan-interactive nil)
	(enwc-scan-internal)))
  (let ((nets (or networks enwc-last-scan))
	need-break cur-net)
    (if (not nets)
	(setq nets enwc-last-scan))
    (while (and nets (not need-break))
      (let (cur-essid)
	(setq cur-net (pop nets))
	(setq cur-essid (cdr (assoc "essid" cur-net)))
	(if (string= cur-essid essid)
	    (setq need-break t))))
    (if need-break
	(if (called-interactively-p 'any)
	    (message (number-to-string (cdr (assoc "id" cur-net))))
	  (cdr (assoc "id" cur-net)))
      (if (called-interactively-p 'any)
	  (message "Network not found.")
	nil))))

;;;;;;;;;;;;;;;;;;;;
;; Connect Network
;;;;;;;;;;;;;;;;;;;;

(defun enwc-connect-network (id)
  "Connect to network with id ID.
This is an entry point for the internal connection functions,
and checks whether or not ENWC is using wired."
  (let (cur-net)
    (if enwc-using-wired
	(progn
	  (enwc-wired-connect id)
	  (setq cur-net (nth id (enwc-get-wired-profiles))))
      (enwc-wireless-connect id)
      (if enwc-last-scan
	  (setq cur-net (cdr (assoc "essid" (nth id enwc-last-scan)))))
    cur-net)))

(defun enwc-connect-to-network (net-id)
  "Connects the the network with network id NET-ID.
Confirms that NET-ID is a valid network id.
This calls `enwc-connect-network' as a subroutine."
  (interactive "nNetwork ID: ")
  (if (not (numberp net-id))
      (error "NET-ID must be a number"))
  (let ((num-ids (length enwc-last-scan))
	cur-net)
    (if (or (< net-id 0) (>= net-id num-ids))
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
  (if (not (eq major-mode 'enwc-mode))
      (enwc-setup-buffer))
  (let ((id (- (line-number-at-pos) 1)))
    (enwc-connect-to-network id)))

(defun enwc-disconnect ()
  "Disconnects from the network, if any."
  (interactive)
  (if (not (eq major-mode 'enwc-mode))
      (enwc-setup-buffer))
  (if enwc-using-wired
      (enwc-wired-disconnect)
    (enwc-wireless-disconnect)))

(defun enwc-toggle-wired ()
  "Toggle the display and mode between wireless and wired.
This function also sets the variable `enwc-using-wired'."
  (interactive)
  (if (not (eq major-mode 'enwc-mode))
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
  (if (not (get-buffer "*ENWC Edit*"))
      (error "Not editing a network entry."))
  (if (not (eq (current-buffer) (get-buffer "*ENWC Edit*")))
      (switch-to-buffer "*ENWC Edit*"))
  (if (not (widget-at))
      (error "No widget at point"))
  (message (widget-field-value-get (widget-at))))

(defun enwc-display-sec-reqs (widget &rest stuff)
  "Display the security requirements.
This is a callback to the security selection widget.
WIDGET is always the menu drop-down of security types."
  (let (reqs
	(inhibit-read-only t)
	type-wid-list)
    ;; First, erase any of the old ones.
    (goto-char (1+ (widget-get widget :to)))
    (widget-forward 1)
    (while (>= (point) (widget-get widget :to))
      (widget-delete (widget-at))
      (widget-forward 1))
    (goto-char (point-max))
    ;; Then check to see if widget-value is None
    (if (string= (widget-value widget) "None")
	nil
      (setq type-wid-list
	    (cadr (assoc "reqs"
			 (cdr (assoc (widget-value widget)
				     (enwc-get-sec-types enwc-using-wired))))))
      (setq reqs
	    (mapcar (lambda (x)
		      (if (not (eq (length (cdr x)) 0))
			  (widget-create 'editable-field
					 :format (concat "  "
							 (cdr x)
							 ": %v")
					 :secret ?*
					 :keymap 'enwc-edit-field-map
					 :value (or (enwc-get-nw-prop enwc-using-wired
								      enwc-edit-id
								      (car x))
						    ""))))
		    type-wid-list))
      (widget-setup)
      reqs)))

(defun enwc-setup-edit-buffer ()
  "Setup the edit buffer.  This removes the old one if neccessary,
and redisplays the settings from the network profile
 with id `enwc-edit-id', which is set in `enwc-edit-entry-at-point'."
  (if (get-buffer "*ENWC Edit*")
      (kill-buffer "*ENWC Edit*"))
  (with-current-buffer (get-buffer-create "*ENWC Edit*")
    (let ((sec-types (enwc-get-sec-types enwc-using-wired))
	  (nw-info (enwc-get-nw-info enwc-using-wired enwc-edit-id))
	  ip-addr netmask gateway dns-1 dns-2
	  addr-wid net-wid gate-wid
	  dns-1-wid dns-2-wid dns-list
	  type-wid type-wid-list)

      (widget-insert (concat "Settings for access point "
			     (cdr (assoc "essid"
					 (nth enwc-edit-id
					      enwc-last-scan)))
			     "\n"))
      (widget-insert "\n")
      ;; ip
      (widget-insert "IPv4 Settings:\n")
      (setq addr-wid (widget-create 'editable-field
				    :format "  Address: %v"
				    :value (or (assoc "addr" nw-info) "")))
      ;; netmask
      (setq net-wid (widget-create 'editable-field
				   :format "  Netmask: %v"
				   :value (or (assoc "netmask" nw-info) "")))

      ;; gateway
      (setq gate-wid (widget-create 'editable-field
				    :format "  Gateway: %v"
				    :value (or (assoc "gateway" nw-info) "")))
      ;; dns1
      (widget-insert "\n")
      ;;(setq dns-list (enwc-get-dns enwc-using-wired enwc-edit-id))
      (setq dns-1-wid (widget-create 'editable-field
				     :format "    DNS 1: %v"
				     :value (or (assoc "dns1" nw-info) "")))

      ;; dns2
      (setq dns-2-wid (widget-create 'editable-field
				     :format "    DNS 2: %v"
				     :value (or (assoc "dns2" nw-info) "")))

      (widget-insert "\n")
      (widget-insert "Security:\n")
      (setq type-wid (apply 'widget-create
      			    'menu-choice
       			    :tag "Type "
			    :value (or (assoc "enctype" nw-info) "None")
			    :notify 'enwc-display-sec-reqs
       			    '(item :tag "No Encryption"
				   :value "None")
			    (mapcar (lambda (x)
				      `(item :format "%t\n"
					     :value ,(car x)
					     :tag ,(cdr (assoc "Name" (cdr x)))))
       			     	    sec-types)))
      (enwc-display-sec-reqs type-wid)
      (use-local-map enwc-edit-map)
      (widget-setup)))

  (switch-to-buffer "*ENWC Edit*"))

(defun enwc-edit-save ()
  "Save the network settings."
  ;; Basically, just iterate through the widgets,
  ;; retrieving values from each.
  (interactive)
  (if (not (get-buffer "*ENWC Edit*"))
      (error "Not editing a network entry."))
  (if (not (eq (current-buffer) (get-buffer "*ENWC Edit*")))
      (switch-to-buffer "*ENWC Edit*"))
  (goto-char 0)
  (let (settings start-pos type-wid-list)

    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "addr"
			      (widget-field-value-get (widget-at)))
			nil)))
    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "netmask"
			      (widget-field-value-get (widget-at)))
			nil)))
    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "gateway"
			      (widget-field-value-get (widget-at)))
			nil)))
    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "dns1"
			      (widget-field-value-get (widget-at)))
			nil)))
    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "dns2"
			      (widget-field-value-get (widget-at)))
			nil)))
    (widget-forward 1)
    (setq settings
	  (append settings
		  (cons (cons "enctype"
			      (widget-value (widget-at)))
			nil)))
    (setq start-pos (widget-get (widget-at) :to))
    (if (not (string= (widget-value (widget-at)) "None"))
	(setq type-wid-list
	      (cadr (assoc "reqs"
			   (cdr (assoc (widget-value (widget-at))
				       (enwc-get-sec-types enwc-using-wired)))))))
    (dolist (x type-wid-list)
      (widget-forward 1)
      (if (not (string= (widget-field-value-get (widget-at)) ""))
	  (setq settings
		(append settings
			(cons (cons (car x)
				    (widget-field-value-get (widget-at)))
			      nil)))))
    ;;(print settings)
    (enwc-save-nw-settings enwc-using-wired enwc-edit-id settings)))

(defun enwc-edit-entry-at-point ()
  "Edit the current network entry."
  (interactive)
  (setq enwc-edit-id (- (line-number-at-pos) 1))
  (select-window (split-window))
  (enwc-setup-edit-buffer))

(defvar enwc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'enwc-scan)
    (define-key map (kbd "C") 'enwc-connect-to-network-essid)
    (define-key map (kbd "D") 'enwc-disconnect)
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
  (if (not (get-buffer "*ENWC*"))
      (with-current-buffer (get-buffer-create "*ENWC*")
	(enwc-mode)))
  (if (not nomove)
      (switch-to-buffer "*ENWC*")))

(provide 'enwc)

;;; enwc.el ends here
