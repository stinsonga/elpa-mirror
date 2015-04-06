;; debbugs-reference.el --- use debbugs-gnu browsing bug references

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.de>
;; Keywords: comm, hypermedia, maint
;; Package: debbugs
;; Version: 0.6

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides minor modes for putting clickable overlays on
;; references to bugs.  It uses bug-reference.el, but changes
;; buffer-local the browser to present the bugs to `debbugs-gnu-bugs'.

;;; Code:

;;;###autoload
(defun debbugs-browse-url (url &optional _new-window)
  (when (and (stringp url) (string-match "[[:digit:]]+$" url))
    (debbugs-gnu-bugs (string-to-number (match-string 0 url)))))

;;;###autoload
(define-minor-mode debbugs-reference-mode
  "Toggle hyperlinking bug references in the buffer (Bug Reference mode).
With a prefix argument ARG, enable Bug Reference mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil."
  nil
  ""
  nil
  (if debbugs-reference-mode
	(setq-local browse-url-browser-function 'debbugs-browse-url)
    (kill-local-variable 'browse-url-browser-function)))

;;;###autoload
(define-minor-mode debbugs-reference-prog-mode
  "Like `debbugs-reference-mode', but only buttonize in comments and strings."
  nil
  ""
  nil
  (if debbugs-reference-prog-mode
	(setq-local browse-url-browser-function 'debbugs-browse-url)
    (kill-local-variable 'browse-url-browser-function)))

(provide 'debbugs-reference)
;;; debbugs-reference.el ends here
