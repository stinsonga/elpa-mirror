;;; enwc-backend.el --- Back-end functions for ENWC.

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

;;; Code:

(require 'map)

(cl-defstruct enwc-backend
  key  ;; The symbol that identifies this backend.
  ;; Loading/unloading functions
  can-load-p
  load
  unload
  ;; Scan interface
  network-ids
  scan
  wireless-nw-props
  ;; Connect/disconnect
  connect
  disconnect
  ;; Maintenance
  current-nw-id
  is-connecting-p
  is-wired-p
  ;; get-profile-info
  ;; save-nw-settings
  )

(defvar enwc-registered-backend-alist nil
  "Alist of registered ENWC backends.

Each entry in this list is of the form

    (KEY . DEFINITION)

Where KEY is a symbol identifying the backend, and DEFINITION is
an enwc-backend struct.")

(defcustom enwc-default-backend nil
  "Key of the default backend to use."
  :type 'symbol
  :group 'enwc)

(defvar enwc--current-backend nil)

(defun enwc-register-backend (definition &optional forcep)
  "Register the backend KEY with DEFINITION.

Signals an error if a backend with KEY already exists and FORCEP is nil."
  (cl-check-type definition enwc-backend)
  (let ((key (enwc-backend-key definition)))
    (when (and (map-contains-key enwc-registered-backend-alist key) (not forcep))
      (error "An ENWC backend with key '%s' has already been registered." key))
    (map-put enwc-registered-backend-alist key definition)))

;; Convenience functions to work with backends

(defun enwc--can-load-p (backend)
  (funcall (enwc-backend-can-load-p backend)))

(defun enwc--load (backend)
  (funcall (enwc-backend-load backend)))

(defun enwc--unload (backend)
  (funcall (enwc-backend-unload backend)))

(defun enwc--scan (backend)
  (funcall (enwc-backend-scan backend)))

(defun enwc--network-ids (backend)
  (funcall (enwc-backend-network-ids backend)))

(defun enwc--wireless-nw-props (backend id)
  (funcall (enwc-backend-wireless-nw-props backend) id))

(defun enwc--connect (backend id &optional wired-p)
  (funcall (enwc-backend-connect backend) id wired-p))

(defun enwc--disconnect (backend &optional wired-p)
  (funcall (enwc-backend-disconnect backend) wired-p))

(defun enwc--current-nw-id (backend &optional wired-p)
  (funcall (enwc-backend-current-nw-id backend) wired-p))

(defun enwc--is-connecting-p (backend)
  (funcall (enwc-backend-is-connecting-p backend)))

(defun enwc--is-wired-p (backend)
  (funcall (enwc-backend-is-wired-p backend)))

;; Handle loading/unloading

(defun enwc-load-backend (backend)
  "Load BACKEND, which is a symbol denoting the backend to use."
  (interactive
   (list
    (intern (completing-read "Backend: " enwc-registered-backend-alist nil t))))
  (when (and enwc--current-backend
             (not (eq (enwc-backend-key enwc--current-backend) backend)))
    (enwc-unload-current-backend))

  (let ((new-backend (map-elt enwc-registered-backend-alist backend)))
    (unless new-backend
      (error "No registered backend %s" backend))

    (unless (enwc--can-load-p new-backend)
      (error "Backend %s is not usable." backend))

    (enwc--load new-backend)
    (setq enwc--current-backend new-backend)))

(defun enwc-load-default-backend ()
  (enwc-load-backend (map-elt enwc-registered-backend-alist enwc-default-backend)))

(defun enwc-unload-current-backend ()
  "Unload the current backend."
  (when enwc--current-backend
    (enwc--unload enwc--current-backend)
    (setq enwc--current-backend nil)))

(provide 'enwc-backend)

;;; enwc-backend.el ends here
