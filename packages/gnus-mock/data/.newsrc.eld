;; -*- mode:emacs-lisp; coding: utf-8-emacs; -*-
;; Gnus startup file.
;; Never delete this file -- if you want to force Gnus to read the
;; .newsrc file (if you have one), touch .newsrc instead.
(setq gnus-newsrc-file-version "Gnus v5.13")

(setq gnus-newsrc-last-checked-date '"Tue, 01 May 2018 12:15:27 -0700")

(setq gnus-newsrc-alist '(("nndraft:queue" 3 nil nil (nndraft "")) ("Welcome" 3 ((1)) nil) ("mails" 3 nil nil) ("incoming" 3 nil nil nil) ("nndraft:drafts" 1 nil nil (nndraft "") ((gnus-dummy (gnus-draft-mode))))))

(setq gnus-server-alist '(("archive" nnfolder "archive" (nnfolder-directory "REPLACE_ME/archive") (nnfolder-active-file "REPLACE_ME/archive/active") (nnfolder-get-new-mail nil) (nnfolder-inhibit-expiry t))))
