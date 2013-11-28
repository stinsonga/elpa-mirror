;;; yas-doc-helper.el --- Help generate documentation for YASnippet

;; Copyright (C) 2012  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some functions to help generate YASnippet docs

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'org)
(require 'org-publish)
(require 'yasnippet) ; docstrings must be loaded

(defun yas--document-symbol (symbol level)
  (flet ((concat-lines (&rest lines)
                       (mapconcat #'identity lines "\n")))
    (let* ((stars (make-string level ?*))
           (heading (cond ((fboundp symbol)
                           (format "%s =%s= (%s)"
                                   stars
                                   symbol
                                   (mapconcat #'symbol-name
                                              (help-function-arglist symbol t) " ")))
                          (t
                           (format "%s =%s=\n" stars symbol))))
           (after-heading
            (concat-lines ":PROPERTIES:"
                          (format ":CUSTOM_ID: %s" symbol)
                          ":END:"))
           (body (or (cond ((boundp symbol)
                            (documentation-property symbol 'variable-documentation t))
                           ((fboundp symbol)
                            (documentation symbol t))
                           (t
                            (format "*WARNING*: no symbol named =%s=" symbol)))
                     (format "*WARNING*: no doc for symbol =%s=" symbol)))
           (case-fold-search nil))
      ;; do some transformations on the body: FOO becomes /foo/ and
      ;; `bar' becomes [[#bar][=bar=]]
      (setq body (replace-regexp-in-string
                  "[A-Z][A-Z-]+" #'(lambda (match)
                                     (format "/%s/" (downcase match)))
                  body)
            body (replace-regexp-in-string "`\\([a-z-]+\\)'" #'(lambda (match)
                                                                 (let* ((name (downcase (match-string 1 match)))
                                                                        (sym (intern name)))
                                                                   (if (and (or (boundp sym)
                                                                                (fboundp sym))
                                                                            (save-match-data
                                                                              (string-match "^yas-" name)))
                                                                       (format "[[#%s][=%s=]]"
                                                                               name name)
                                                                     (format "=%s=" name))))
                                           body))
      ;; output the paragraph
      ;;
      (concat-lines heading
                    after-heading
                    body))))

(defun yas--document-symbols (level &rest names-and-predicates)
  (let ((sym-lists (make-vector (length names-and-predicates) nil)))
    (loop for sym in yas--exported-syms
          do (loop for test in (mapcar #'cdr names-and-predicates)
                   for i from 0
                   do (when (funcall test sym)
                        (push sym (aref sym-lists i))
                        (return))))
    (loop for slist across sym-lists
          for name in (mapcar #'car names-and-predicates)
          concat (format "\n** %s\n" name)
          concat (mapconcat (lambda (sym)
                              (yas--document-symbol sym (1+ level)))
                            slist "\n\n"))))

(defun yas--internal-link-snippet ()
  (interactive)
  (yas-expand-snippet "[[#$1][=${1:`yas/selected-text`}=]]"))

(define-key org-mode-map [M-f8] 'yas--internal-link-snippet)

;; This lets all the org files be exported to HTML with
;; `org-publish-current-project' (C-c C-e P).

(let* ((rev (or (with-temp-buffer
                  (when (eq (call-process "git" nil t nil
                                          "rev-parse" "--verify" "HEAD") 0)
                    (buffer-string)))
                yas--version))
       (dir (if load-file-name (file-name-directory load-file-name)
              default-directory))
       (proj-plist
        (list
         :base-directory dir :publishing-directory dir
         :html-postamble
         (concat "<hr><p class='creator'>Generated by %c on %d from "
                 rev "</p>\n"
                 "<p class='xhtml-validation'>%v</p>\n")))
       (project (assoc "yasnippet" org-publish-project-alist)))
  (if project
      (setcdr project proj-plist)
    (push `("yasnippet" . ,proj-plist)
          org-publish-project-alist)))

(defun yas--generate-html-batch ()
  (let ((org-publish-use-timestamps-flag nil)
        (org-export-copy-to-kill-ring nil)
        (org-confirm-babel-evaluate nil)
        (make-backup-files nil))
    (org-publish "yasnippet" 'force)))



(provide 'yas-doc-helper)
;;; yas-doc-helper.el ends here
;; Local Variables:
;; coding: utf-8
;; End:
