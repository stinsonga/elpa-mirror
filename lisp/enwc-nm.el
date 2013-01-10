;;; enwc-nm.el - The NetworkManager backend to ENWC

;; Copyright (C) 2012 Ian Dunn

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


;;; Commentary:
;;
;;   This requires NetworkManager >= 0.9.6
;;

(require 'enwc)

(defgroup enwc-nm nil
  "*NetworkManager variables for ENWC"
  :prefix "enwc-nm-"
  :group 'enwc)

(defcustom enwc-nm-dbus-service "org.freedesktop.NetworkManager"
  "NetworkManager D-Bus service."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-path "/org/freedesktop/NetworkManager"
  "The default D-Bus path for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-settings-path "/org/freedesktop/NetworkManager/Settings"
  "The settings D-Bus path for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-interface "org.freedesktop.NetworkManager"
  "The default D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-wireless-interface "org.freedesktop.NetworkManager.Device.Wireless"
  "The wireless D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-wired-interface "org.freedesktop.NetworkManager.Device.Wired"
  "The wired D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-device-interface "org.freedesktop.NetworkManager.Device"
  "The device D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-accesspoint-interface "org.freedesktop.NetworkManager.AccessPoint"
  "The access point D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-settings-interface "org.freedesktop.NetworkManager.Settings"
  "The settings D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-connections-interface "org.freedesktop.NetworkManager.Settings.Connection"
  "The connections D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defcustom enwc-nm-dbus-active-interface "org.freedesktop.NetworkManager.Connection.Active"
  "The active connection D-Bus interface for NetworkManager."
  :group 'enwc-nm
  :type 'string)

(defvar enwc-nm-details-list
  '("Ssid" "HwAddress" "Strength" "Flags" "Mode" "Frequency")
  "The list of the desired details to be obtained from each network.")

(defvar enwc-nm-sec-types
  '(("eap-leap" . (("Name" . "eap-leap")
		   ("reqs" . (("identity" . "Username")
			      ("password" . "Password")))))
    ("eap-peap" . (("Name" . "eap-peap")
		   ("reqs" . (("anonymous-identity" . "Anonymous Identity")
			      ("ca-cert" . "CA Certificate")
			      ("phase2-auth" . "Inner Authentication")
			      ("phase1-peapver" . "PEAP Version")
			      ("identity" . "Username")
			      ("password" . "Password")))))
    ("eap-tls" . (("Name" . "eap-tls")
		  ("reqs" . (("identity" . "Identity")
			     ("client-cert" . "User Certificate")
			     ("ca-cert" . "CA Certificate")
			     ("private-key" . "Private Key")
			     ("private-key-password" . "Private Key Password")))))
    ("eap-ttls" . (("Name" . "eap-ttls")
		   ("reqs" . (("anonymous-identity" . "Anonymous Identity")
			      ("ca-cert" . "CA Certificate")
			      ("phase2-auth" . "Inner Authentication")
			      ("identity" . "Username")
			      ("password" . "Password")))))
    ("wpa-psk" . (("Name" . "wpa2")
		  ("reqs" . (("psk" . "PSK")))))
    ("wep" . (("Name" . "wep")
	      ("reqs" . (("wep-key0" . "WEP Key")
			 ("wep-key-type" . "WEP Key Type")))))
    ("leap" . (("Name" . "leap")
	       ("reqs" . (("leap-username" . "Username")
			  ("leap-password" . "Password")))))
    )
  "The security types for NetworkManager.
This is still in the process of being worked on."
  )

(defvar enwc-nm-wired-dev nil
  "The wired device object path.")

(defvar enwc-nm-wireless-dev nil
  "The wireless device object path.")

(defvar enwc-nm-active-ap nil
  "The active access point object path.")

(defvar enwc-nm-connecting-p nil
  "Whether or not NetworkManager is connecting.")

(defvar enwc-nm-wired-p nil
  "Whether or not NetworkManager is wired.")

(defun enwc-nm-get-settings (conn)
  "Gets the connection settings.
CONN is an object path to the connection."
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    conn
		    enwc-nm-dbus-connections-interface
		    "GetSettings"
		    :timeout 25000))
  

(defun enwc-nm-list-connections ()
  "List the connections."
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-dbus-settings-path
		    enwc-nm-dbus-settings-interface
		    "ListConnections"
		    :timeout 25000))

;; Default
(defun enwc-nm-get-device-by-name (name)
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-dbus-path
		    enwc-nm-dbus-interface
		    "GetDeviceByIpIface"
		    :timeout 25000
		    :string name))

;; Settings, Connections
(defun enwc-nm-get-uuid-by-ssid (ssid)
  "Gets the uuid of the network with ssid SSID."
  (let ((conns (enwc-nm-list-connections))
	cur-conn cur-ssid uuid)
    (while (and conns (not uuid))
      (setq cur-conn (pop conns))
      (let ((settings (enwc-nm-get-settings cur-conn)))
	(if (not (assoc "802-11-wireless" settings))
	    nil
	  (setq cur-ssid
		(dbus-byte-array-to-string (car (cadr (assoc "ssid"
							     (cadr (assoc "802-11-wireless" settings)))))))
	  (if (string= cur-ssid ssid)
	      (setq uuid (car (cadr (assoc "uuid" (cadr (assoc "connection" settings))))))))))))

(defun enwc-nm-get-uuid-by-id (id)
  (let ((conns (enwc-nm-list-connections))
	cur-conn cur-id uuid)
    (while (and conns (not uuid))
      (setq cur-conn (pop conns))
      (let ((settings (enwc-nm-get-settings cur-conn))
	    conn-set)
	(if (not (assoc "connection" settings))
	    nil
	  (setq conn-set (assoc "connection" settings))
	  (setq cur-id (car (cadr (assoc "id" (cadr conn-set)))))
	  (if (string= cur-id id)
	      (setq uuid (car (cadr (assoc "uuid" (cadr conn-set)))))))))))

(defun enwc-nm-get-conn-by-uuid (uuid)
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-dbus-settings-path
		    enwc-nm-dbus-settings-interface
		    "GetConnectionByUuid"
		    :timeout 25000
		    uuid))

;; Settings
(defun enwc-nm-get-conn-by-ssid (ssid)
  "Gets the connection path for the access point with ssid SSID."
  (let ((uuid (enwc-nm-get-uuid-by-ssid ssid)))
    (enwc-nm-get-conn-by-uuid uuid)))

(defun enwc-nm-get-conn-by-id (id)
  (let ((uuid (enwc-nm-get-uuid-by-id id)))
    (enwc-nm-get-conn-by-uuid uuid)))


;; Wireless
(defun enwc-nm-scan ()
  "The NetworkManager scan function."
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-wireless-dev
		    enwc-nm-dbus-wireless-interface
		    "RequestScan"
		    :timeout 25000
		    '(:array :signature "{sv}")))

;; Wireless
(defun enwc-nm-get-networks ()
  "The NetworkManager get networks function.
This returns a list of D-Bus paths to the access points."
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-wireless-dev
		    enwc-nm-dbus-wireless-interface

		    "GetAccessPoints"))

;; Accesspoint
(defun enwc-nm-get-wireless-network-property (id prop)
  "The NetworkManager get wireless network property function.
This runs like normal, using element ID of `enwc-access-points'
 to determine the access point path, then obtains the property
PROP from that access point.  It also sets the channel from the
 frequency if necessary."
  (let ((ap (nth id enwc-access-points))
	ret)
    (setq ret (dbus-get-property :system
				 enwc-nm-dbus-service
				 ap
				 enwc-nm-dbus-accesspoint-interface
				 prop))
    (if (string= prop "Frequency")
	(setq ret (number-to-string (1+ (/ (- ret 2412) 5)))))
    (if (string= prop "Ssid")
	(setq ret (dbus-byte-array-to-string ret)))
    (if (string= prop "Mode")
	(setq ret (cond ((= ret 0) "Unknown")
			((= ret 1) "Ad-Hoc")
			((= ret 2) "Infrastructure"))))
    ret))

(defun enwc-nm-get-encryption-type (id)
  "The NetworkManager get encryption type function.
This gets the WPA flags and RSN flags from access point in ID.
If both are 0, then it returns WEP, otherwise WPA."
  (let ((wpa-flags (enwc-nm-get-wireless-network-property id "WpaFlags"))
	(rsn-flags (enwc-nm-get-wireless-network-property id "RsnFlags")))
    (if (and (= wpa-flags 0) (= rsn-flags 0))
	"WEP"
      "WPA")))

;; Default
(defun enwc-nm-connect (id)
  "The NetworkManager connect function.
This gets the connection path from ID, and connects to it."
  (let ((ssid (enwc-nm-get-wireless-network-property id "Ssid")))
    (dbus-call-method :system
		      enwc-nm-dbus-service
		      enwc-nm-dbus-path
		      enwc-nm-dbus-interface
		      "ActivateConnection"
		      :timeout 25000
		      :object-path (enwc-nm-get-conn-by-ssid ssid)
		      :object-path enwc-nm-wireless-dev
		      :object-path (nth id enwc-access-points))))


(defun enwc-nm-string-idx (obj list)
  (let ((tmp-list list)
	(cur-pos -1)
	cur-obj)
    (while (and (not cur-obj) tmp-list)
      (setq cur-obj (pop tmp-list))
      (setq cur-pos (1+ cur-pos))
      (if (not (string= obj cur-obj))
	  (setq cur-obj nil)))
    cur-pos))

(defun enwc-nm-wireless-prop-changed (props)
  (if (assoc "ActiveAccessPoint" props)
      (setq enwc-nm-active-ap (car (cadr (assoc "ActiveAccessPoint"
						props))))))

;; Wireless
(defun enwc-nm-get-current-nw-id (wired)
  "The NetworkManager get current network id function.
This simply checks for the active access point."
  (if (or wired (string= enwc-nm-active-ap "/"))
      -1
    (enwc-nm-string-idx enwc-nm-active-ap enwc-access-points)))

(defun enwc-nm-prop-changed (state)
  (setq enwc-nm-connecting-p (eq state 40)))

;; Default
(defun enwc-nm-check-connecting ()
  "The NetworkManager check connecting function."
  enwc-nm-connecting-p)

;; Device
(defun enwc-nm-disconnect-wireless ()
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-wireless-dev
		    enwc-nm-dbus-device-interface
		    "Disconnect"))

;; Settings, Connections
(defun enwc-nm-get-wired-profiles ()
  (let ((profs-list (enwc-nm-list-connections)))
    (mapcar (lambda (x)
	      (let ((props (enwc-nm-get-settings x)))
		(if (string= (caar props) "connection")
		    (car (cadr (car (cadr (car props))))))))
	    profs-list)))

(defun enwc-nm-wired-connect (id)
  (let ((nid (nth id enwc-access-points)))
    (dbus-call-method :system
		      enwc-nm-dbus-service
		      enwc-nm-dbus-path
		      enwc-nm-dbus-interface
		      :timeout 25000
		      :object-path (enwc-nm-get-conn-by-id nid)
		      :object-path enwc-nm-wired-dev
		      :objec-path "/")))

;; Device
(defun enwc-nm-wired-disconnect ()
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    enwc-nm-wired-dev
		    enwc-nm-dbus-device-interface
		    "Disconnect"))

(defun enwc-nm-dev-prop-changed (new-state old-state reason)
  (setq enwc-nm-wired-p (eq new-state 100)))

;; Default
(defun enwc-nm-is-wired ()
  enwc-nm-wired-p)

;; Each entry in sec-types should be:
;; ("IDENT" (("Name" . "NAME") ("reqs" . (("key1" . "Entry1") ("key2" . "Entry2") ... ))))
;; Where:
;;  "IDENT" => String that identifies this to the backend.
;;  "NAME" => String that ENWC displays
;;  "reqs" => Constant string, but the association list holds entries
;;              required by the security type, i.e. user, passphrase, etc.
;;  "keyXX" => String that the backend uses for this security entry.
;;  "EntryXX" => String that ENWC displays for this security entry.

(defun enwc-nm-get-sec-types (wired)
  "Get security types."
  (if wired
      nil
    nil)
  )

(defun enwc-nm-convert-addr (addr)
  (let* ((hex-addr (format "%08x" addr))
	 (ret-addr (format "%i.%i.%i.%i"
			   (string-to-number (substring hex-addr
							6 8)
					     16)
			   (string-to-number (substring hex-addr
							4 6)
					     16)
			   (string-to-number (substring hex-addr
							2 4)
					     16)
			   (string-to-number (substring hex-addr
							0 2)
					     16))))
    ret-addr))

(defun enwc-nm-addr-back (addr)
  (let ((bytes (split-string addr "."))
	 ret-val)
    (setq ret-val (logior (lsh (string-to-number (nth 3 bytes))
			       0)
			  (lsh (string-to-number (nth 2 bytes))
			       1)
			  (lsh (string-to-number (nth 1 bytes))
			       2)
			  (lsh (string-to-number (nth 0 bytes))
			       3)))))

;; These next two come from libnm-util/nm-utils.c in NM's source.

(defun enwc-nm-netmask-to-prefix (netmask)
  "Converts a netmask to a prefix.
NETMASK is an ip address in network byte order."
  (let* ((mask netmask)
	 (cur-pos 3)
	 (cur-mark (logand (lsh mask (* -8 cur-pos)) 255))
	 (pf 0))
    (while (and (eq cur-mark 255) (>= cur-pos 0))
      (setq pf (+ pf 8))
      (setq cur-pos (1- cur-pos))
      (setq cur-mark (logand (lsh mask (* -8 cur-pos)) 255))
    )

    (if (>= cur-pos 0)
	(let ((v (logand (lsh mask (* -8 cur-pos)) 255)))
	  (while (not (eq v 0))
	    (setq pf (1+ pf))
	    (setq v (lsh v 1)))))
    pf))

(defun enwc-nm-prefix-to-netmask (prefix)
  "Converts a prefix to a netmask.
PREFIX is an integer <= 32."
  (let ((pf prefix)
	(netmask 0)
	(msk #x80000000))
  (while (> pf 0)
    (setq netmask (logior netmask msk))
    (setq msk (lsh msk -1))
    (setq pf (1- pf)))
  netmask))

(defun enwc-nm-get-ip-addr (wired id)
  "Gets the IP Address of a connection profile."
  (let ((props (enwc-nm-get-settings (nth id enwc-access-points)))
	ipaddr ret-addr)
    (setq ipaddr (nth 0 (caar (cadr (assoc "addresses"
					   (cadr (assoc "ipv4"
							props)))))))
    (setq ret-addr (enwc-nm-convert-addr ipaddr))))

(defun enwc-nm-get-netmask (wired id)
  "Gets the Netmask of a connection profile."
  (let ((props (enwc-nm-get-settings (nth id enwc-access-points)))
	ipaddr hex-addr ret-addr)
    (setq ipaddr (nth 3 (caar (cadr (assoc "addresses"
					   (cadr (assoc "ipv4"
							props)))))))
    (setq hex-addr (enwc-nm-prefix-to-netmask ipaddr))
    (setq ret-addr (format "%i.%i.%i.%i"
			   (logand hex-addr 255)
			   (logand (lsh hex-addr -8) 255)
			   (logand (lsh hex-addr -16) 255)
			   (logand (lsh hex-addr -24) 255)))))
    

(defun enwc-nm-get-gateway (wired id)
  "Gets the Gateway of a connection profile."
  (let ((props (enwc-nm-get-settings (nth id enwc-access-points)))
	ipaddr ret-addr)
    (setq ipaddr (nth 2 (caar (cadr (assoc "addresses"
					   (cadr (assoc "ipv4"
							props)))))))
    (setq ret-addr (enwc-nm-convert-addr ipaddr))))

(defun enwc-nm-get-dns (wired id)
  "Gets the DNS settings of a connection profile."
  (let ((props (enwc-nm-get-settings (nth id enwc-access-points)))
	dns-list)
    (setq dns-list (car (cadr (assoc "dns" (cadr (assoc "ipv4"
							props))))))
    (mapcar 'enwc-nm-convert-addr
	    dns-list)))

(defun enwc-nm-finalize-settings (settings)
  "Sets up all of the D-BUS types of a settings list.
SETTINGS is the list of settings list to setup.
This will place all of the necessary markers in the list,
such as :array, :dict-entry, etc."
  (cons :array
	(let (first-one)
	  (mapc (lambda (x)
		  (setq first-one (cons :dict-entry first-one))
		  (setq first-one
			(cons (cons :string
				    (cons (car x)
					  (cons (cons :array
						      (let (this-one)
							(mapc (lambda (y)
								(setq this-one
								      (cons :dict-entry this-one))
								(setq this-one
								      (cons (list :string (car y)
										  :variant
										  (if (string= (car y) "ssid")
										      (cons (dbus-string-to-byte-array
											     (dbus-byte-array-to-string (car (cadr y))))
											    nil)
										    (cadr y)))
									    this-one)))
							      (cadr x))
							(nreverse this-one))) nil)))
			      first-one)))
		settings)
	  (nreverse first-one))))


(defun enwc-nm-setup-settings (id settings)
  (let ((props (enwc-nm-get-settings (nth id enwc-access-points)))
	ret-list)
    (setcdr (assoc "addresses" (cadr (assoc "ipv4" props)))
	    (list (list (list (list (enwc-nm-addr-back
				     (cdr (assoc "addr" settings)))
				    (enwc-nm-netmask-to-prefix (enwc-nm-addr-back
								(cdr (assoc "netmask"
									    settings))))
				    (enwc-nm-addr-back
				     (cdr (assoc "gateway" settings))))))))
    (setcdr (assoc "dns" (cadr (assoc "ipv4" props)))
	    (list (list (list (enwc-nm-addr-back
			       (cdr (assoc "dns1" settings)))
			      (enwc-nm-addr-back
			       (cdr (assoc "dns2" settings)))))))
    ))

(defun enwc-nm-save-nw-settings (wired id settings)
  "Saves network settings.
ID is the network id of the profile to save,
WIRED denotes whether or not this is a wired profile,
and SETTINGS is the list of settings."
  (dbus-call-method :system
		    enwc-nm-dbus-service
		    (nth id enwc-access-points)
		    enwc-nm-dbus-connections-interface
		    :timeout 25000
		    "Update"
		    (enwc-nm-setup-settings id settings)))

(defun enwc-nm-setup ()
  (setq enwc-nm-wired-dev (enwc-nm-get-device-by-name enwc-wired-device)
	enwc-nm-wireless-dev (enwc-nm-get-device-by-name enwc-wireless-device))

  (dbus-register-signal :system
			enwc-nm-dbus-service
			enwc-nm-wireless-dev
			enwc-nm-dbus-wireless-interface
			"AccessPointAdded"
			'enwc-process-scan)

  (dbus-register-signal :system
			enwc-nm-dbus-service
			enwc-nm-wireless-dev
			enwc-nm-dbus-wireless-interface
			"AccessPointRemoved"
			'enwc-process-scan)

  (setq enwc-nm-active-ap
	(let ((cur-net (dbus-get-property :system
					  enwc-nm-dbus-service
					  enwc-nm-wireless-dev
					  enwc-nm-dbus-wireless-interface
					  "ActiveAccessPoint")))
	  (if (string= cur-net "/")
	      -1
	    cur-net)))

  (dbus-register-signal :system
			enwc-nm-dbus-service
			enwc-nm-wireless-dev
			enwc-nm-dbus-wireless-interface
			"PropertiesChanged"
			'enwc-nm-wireless-prop-changed)

  (setq enwc-nm-connecting-p
	(let ((state (dbus-get-property :system
					enwc-nm-dbus-service
					enwc-nm-dbus-path
					enwc-nm-dbus-interface
					"State")))
	  (eq state 40)))

  (dbus-register-signal :system
			enwc-nm-dbus-service
			enwc-nm-wired-dev
			enwc-nm-dbus-device-interface
			"StateChanged"
			'enwc-nm-dev-prop-changed)

  (setq enwc-nm-wired-p
	(let ((state (dbus-get-property :system
					enwc-nm-dbus-service
					enwc-nm-wired-dev
					enwc-nm-dbus-device-interface
					"State")))
	  (eq state 100)))
  (dbus-register-signal :system
			enwc-nm-dbus-service
			enwc-nm-dbus-path
			enwc-nm-dbus-interface
			"StateChanged"
			'enwc-nm-prop-changed)
  )


(provide 'enwc-nm)

;;; End of File.
