;;; enwc-wpa.el --- Emacs Network Client WPA_CLI backend

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: external, network, wicd, manager, nm
;; Version: 2.0
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
;; Back-end driven by wpa_cli
;;
;; Requires root to use wpa_cli

;;; Code:

(require 'enwc)

(defgroup enwc-wpa nil
  "Settings for wpa_cli back-end."
  :prefix "enwc-wpa-"
  :group 'enwc)

(defcustom enwc-wpa-wpa-cli-program "/usr/bin/wpa_cli"
  "Name of the wpa_cli executable."
  :group 'enwc-wpa
  :type 'file)

(defvar enwc-wpa--wpa-cli-buffer " *ENWC wpa_cli*")

;; (defun enwc-wpa--run-wpa-cli (&rest args)
;;   ;; Allow us to go into root.
;;   (cd "/sudo::/")
;;   (setq args (append
;;               (list "-i" (if enwc-using-wired enwc-wired-device enwc-wireless-device))
;;               args))
;;   (apply #'call-process "wpa_cli" nil enwc-wpa--wpa-cli-buffer nil args))


;; Load/Unload

(defun enwc-wpa-load ()
  ;; Start the wpa_cli process
  ;; Start the wpa_supplicant process if necessary
)


; ;;;;;;;;;; ;
; ;; Scan ;; ;
; ;;;;;;;;;; ;

(defun enwc-wpa-scan ()
  "Run wpa_cli to get a scan"
  (enwc-wpa--run-wpa-cli "scan")
  ()
  )

;;; enwc-wpa.el ends here
