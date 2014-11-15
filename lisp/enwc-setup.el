;; enwc-setup.el - Setup routines for ENWC

;; Copyright (C) 2012-2014 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: network, wicd, manager, nm
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

;;; Code:

(require 'enwc)
(require 'enwc-wicd)
(require 'enwc-nm)

(defun enwc-load-backend (&optional backend)
  "Load the backend BACKEND."
  (interactive)
  (unless backend
    (setq backend
          (completing-read "Backend: " enwc-backends nil t)))
  (when (and enwc-current-backend
             (not (eq enwc-current-backend backend)))
    (enwc-unload-backend))
  (let ((service (symbol-value (intern (concat "enwc-"
                                               (symbol-name backend)
                                               "-dbus-service")))))
    (unless (dbus-ping :system service)
      (error "Backend %s is not usable." backend))

    (enwc-call-backend-function "load" backend)
    (setq enwc-current-backend backend)))

(defun enwc-unload-backend ()
  "Unload the current backend."
  (when enwc-current-backend
    (enwc-call-backend-function "unload")
    (setq enwc-current-backend nil)))

(defun enwc-setup ()
  "Set up ENWC.
This sets up ENWC and confirms that one of the backends can be found on D-Bus."
  (when enwc-display-mode-line
    (enwc-enable-display-mode-line))

  (when (and enwc-auto-scan
             (> enwc-auto-scan-interval 0)
             (not enwc-scan-timer))
    (setq enwc-scan-timer
          (run-at-time t enwc-auto-scan-interval 'enwc-scan t)))

  (let ((back-list enwc-backends)
        cur-back pass)
    (while (and back-list (not pass))
      (setq cur-back (pop back-list))
      (setq pass (ignore-errors (enwc-load-backend cur-back))))
    (unless pass
      (error "No usable backend was found."))))

(provide 'enwc-setup)

;;; enwc-setup.el ends here
