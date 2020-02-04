;;; vcard.el --- Utilities for working with vCard files  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Free Software Foundation, Inc.

;; Version: 0
;; Package-Requires: ((emacs "25.1"))

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: mail, comm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides libraries for working with vCard data: files
;; representing contact information.  At present there are two parts
;; to it: a major mode for looking at *.vcf files, and a library for
;; parsing those files into elisp data structures.  The third part,
;; eventually, will be a library for writing elisp data structures to
;; *.vcf files.

;;; Code:

(defgroup vcard nil
  "Customization options for the vcard library."
  :group 'mail)

(provide 'vcard)
;;; vcard.el ends here
