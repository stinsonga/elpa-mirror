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

(defun enwc--intern-sym (sym-name suffix)
  (intern (concat "enwc-" sym-name suffix)))

(defun enwc-setup-backend (cur-back)
  "Sets up ENWC to use the correct function for the backend CUR-BACK."
  (let* ((sym-name (symbol-name cur-back))
         (eis (apply-partially 'enwc--intern-sym sym-name)))
    (setq enwc-scan-func (funcall eis "-scan")
          enwc-get-networks-func (funcall eis "-get-networks")
          enwc-get-wireless-nw-props-func (funcall eis "-get-wireless-nw-props")
          enwc-connect-func (funcall eis "-connect")
          enwc-disconnect-func (funcall eis "-disconnect")
          enwc-get-current-nw-id-func (funcall eis "-get-current-nw-id")
          enwc-check-connecting-func (funcall eis "-check-connecting")
          enwc-is-wired-func (funcall eis "-is-wired")
          enwc-get-profile-info-func (funcall eis "-get-profile-info")
          enwc-save-nw-settings-func (funcall eis "-save-nw-settings"))
    (funcall (funcall eis "-setup"))))

(defun enwc-setup ()
  "Sets up ENWC.
This setups ENWC and confirms that one of the backends can be found
on D-Bus."
  (when enwc-display-mode-line
    (enwc-enable-display-mode-line))

  (when (and enwc-auto-scan (> enwc-auto-scan-interval 0))
    (setq enwc-scan-timer
          (run-at-time t enwc-auto-scan-interval 'enwc-scan t)))

  (let ((cur-back nil)
        (back-list enwc-backends))
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
