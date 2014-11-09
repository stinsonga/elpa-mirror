;; enwc-setup.el - Setup routines for ENWC

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


(require 'enwc)
(require 'enwc-wicd)
(require 'enwc-nm)

(defvar enwc-backend-symbol-list
  '("scan"
    "get-networks"
    "get-wireless-nw-props"
    "connect"
    "disconnect"
    "get-current-nw-id"
    "check-connecting"
    "is-wired"
    "get-profile-info"
    "save-nw-settings"))

(defun enwc--intern-sym (sym-name suffix)
  (intern (concat "enwc-" sym-name suffix)))

(defun enwc--setq-backend (backend func)
  "Set a backend function."
  (set (intern (concat "enwc-" func "-func"))
       (intern (concat "enwc-" backend "-" func))))

(defun enwc-setup-backend (cur-back)
  "Sets up ENWC to use the correct function for the backend CUR-BACK."
  (let* ((sym-name (symbol-name cur-back))
         (esb (apply-partially 'enwc--setq-backend sym-name)))
    (cl-mapc esb enwc-backend-symbol-list)
    (funcall (enwc--intern-sym sym-name "-setup"))))

(defun enwc-setup ()
  "Sets up ENWC.
This setups ENWC and confirms that one of the backends can be found
on D-Bus."
  (when enwc-display-mode-line
    (enwc-enable-display-mode-line))

  (when (and enwc-auto-scan (> enwc-auto-scan-interval 0))
    (setq enwc-scan-timer
          (run-at-time t enwc-auto-scan-interval 'enwc-scan t)))

  (let ((back-list enwc-backends)
        cur-back)
    (while (and back-list (not cur-back))
      (setq cur-back (pop back-list))
      (unless (dbus-ping :system
                         (symbol-value (intern (concat "enwc-"
                                                       (symbol-name cur-back)
                                                       "-dbus-service"))))
        (setq cur-back nil)))
    (if cur-back
        (enwc-setup-backend cur-back)
      (error "No usable backend found."))))

(provide 'enwc-setup)

;;; End of File.
