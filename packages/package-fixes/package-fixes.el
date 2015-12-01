;;; package-fixes.el --- package.el bug fixes ported to older Emacsen  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 0

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

;; This package fixes some critical bugs in package.el 1.0.1 which
;; cause bad .elc files to be created during package upgrades when a
;; macro changes.  It is designed to be required as a dependency by
;; packages whose installation is affected by these bugs.

;; This package can be safely installed on recent Emacsen, in which
;; case it does nothing.

;;; Code:

(require 'package)
(require 'find-func)

(unless (fboundp 'package--list-loaded-files)

  (defun package--autoloads-file-name (pkg-desc)
    "Return the absolute name of the autoloads file, sans extension.
PKG-DESC is a `package-desc' object."
    (expand-file-name
     (format "%s-autoloads" (package-desc-name pkg-desc))
     (package-desc-dir pkg-desc)))

  (defun package--activate-autoloads-and-load-path (pkg-desc)
    "Load the autoloads file and add package dir to `load-path'.
PKG-DESC is a `package-desc' object."
    (let* ((old-lp load-path)
           (pkg-dir (package-desc-dir pkg-desc))
           (pkg-dir-dir (file-name-as-directory pkg-dir)))
      (with-demoted-errors "Error loading autoloads: %s"
        (load (package--autoloads-file-name pkg-desc) nil t))
      (when (and (eq old-lp load-path)
                 (not (or (member pkg-dir load-path)
                          (member pkg-dir-dir load-path))))
        ;; Old packages don't add themselves to the `load-path', so we have to
        ;; do it ourselves.
        (push pkg-dir load-path))))

  (defvar warning-minimum-level)
  (defun package--compile (pkg-desc)
    "Byte-compile installed package PKG-DESC."
    (let ((warning-minimum-level :error)
          (save-silently inhibit-message)
          (load-path load-path))
      (package--activate-autoloads-and-load-path pkg-desc)
      (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))

  (defun package--list-loaded-files (dir)
    "Recursively list all files in DIR which correspond to loaded features.
Returns the `file-name-sans-extension' of each file, relative to
DIR, sorted by most recently loaded last."
    (let* ((history (delq nil
                          (mapcar (lambda (x)
                                    (let ((f (car x)))
                                      (and f (file-name-sans-extension f))))
                                  load-history)))
           (dir (file-truename dir))
           ;; List all files that have already been loaded.
           (list-of-conflicts
            (delq
             nil
             (mapcar
              (lambda (x) (let* ((file (file-relative-name x dir))
                            ;; Previously loaded file, if any.
                            (previous
                             (ignore-errors
                               (file-name-sans-extension
                                (file-truename (find-library-name file)))))
                            (pos (when previous (member previous history))))
                       ;; Return (RELATIVE-FILENAME . HISTORY-POSITION)
                       (when pos
                         (cons (file-name-sans-extension file) (length pos)))))
              (directory-files-recursively dir "\\`[^\\.].*\\.el\\'")))))
      ;; Turn the list of (FILENAME . POS) back into a list of features.  Files in
      ;; subdirectories are returned relative to DIR (so not actually features).
      (let ((default-directory (file-name-as-directory dir)))
        (mapcar (lambda (x) (file-truename (car x)))
                (sort list-of-conflicts
                      ;; Sort the files by ascending HISTORY-POSITION.
                      (lambda (x y) (< (cdr x) (cdr y))))))))

  (defun package--load-files-for-activation (pkg-desc reload)
    "Load files for activating a package given by PKG-DESC.
Load the autoloads file, and ensure `load-path' is setup.  If
RELOAD is non-nil, also load all files in the package that
correspond to previously loaded files."
    (let* ((loaded-files-list (when reload
                                (package--list-loaded-files (package-desc-dir pkg-desc)))))
      ;; Add to load path, add autoloads, and activate the package.
      (package--activate-autoloads-and-load-path pkg-desc)
      ;; Call `load' on all files in `package-desc-dir' already present in
      ;; `load-history'.  This is done so that macros in these files are updated
      ;; to their new definitions.  If another package is being installed which
      ;; depends on this new definition, not doing this update would cause
      ;; compilation errors and break the installation.
      (with-demoted-errors "Error in package--load-files-for-activation: %s"
        (mapc (lambda (feature) (load feature nil t))
              ;; Skip autoloads file since we already evaluated it above.
              (remove (file-truename (package--autoloads-file-name pkg-desc))
                      loaded-files-list)))))

  (defun package-activate-1 (pkg-desc &optional reload deps)
    "Activate package given by PKG-DESC, even if it was already active.
If DEPS is non-nil, also activate its dependencies (unless they
are already activated).
If RELOAD is non-nil, also `load' any files inside the package which
correspond to previously loaded files (those returned by
`package--list-loaded-files')."
    (let* ((name (package-desc-name pkg-desc))
           (pkg-dir (package-desc-dir pkg-desc)))
      (unless pkg-dir
        (error "Internal error: unable to find directory for `%s'"
               (package-desc-full-name pkg-desc)))
      ;; Activate its dependencies recursively.
      ;; FIXME: This doesn't check whether the activated version is the
      ;; required version.
      (when deps
        (dolist (req (package-desc-reqs pkg-desc))
          (unless (package-activate (car req))
            (error "Unable to activate package `%s'.\nRequired package `%s-%s' is unavailable"
                   name (car req) (package-version-join (cadr req))))))
      (package--load-files-for-activation pkg-desc reload)
      ;; Add info node.
      (when (file-exists-p (expand-file-name "dir" pkg-dir))
        ;; FIXME: not the friendliest, but simple.
        (require 'info)
        (info-initialize)
        (push pkg-dir Info-directory-list))
      (push name package-activated-list)
      ;; Don't return nil.
      t))

  (defun package-activate (package &optional force)
    "Activate the package named PACKAGE.
If FORCE is true, (re-)activate it if it's already activated.
Newer versions are always activated, regardless of FORCE."
    (let ((pkg-descs (cdr (assq package package-alist))))
      ;; Check if PACKAGE is available in `package-alist'.
      (while
          (when pkg-descs
            (let ((available-version (package-desc-version (car pkg-descs))))
              (or (package-disabled-p package available-version)
                  ;; Prefer a builtin package.
                  (package-built-in-p package available-version))))
        (setq pkg-descs (cdr pkg-descs)))
      (cond
       ;; If no such package is found, maybe it's built-in.
       ((null pkg-descs)
        (package-built-in-p package))
       ;; If the package is already activated, just return t.
       ((and (memq package package-activated-list) (not force))
        t)
       ;; Otherwise, proceed with activation.
       (t (package-activate-1 (car pkg-descs) nil 'deps)))))

  (defun package-unpack (pkg-desc)
    "Install the contents of the current buffer as a package."
    (let* ((name (package-desc-name pkg-desc))
           (dirname (package-desc-full-name pkg-desc))
           (pkg-dir (expand-file-name dirname package-user-dir)))
      (pcase (package-desc-kind pkg-desc)
        (`dir
         (make-directory pkg-dir t)
         (let ((file-list
                (directory-files
                 default-directory 'full "\\`[^.].*\\.el\\'" 'nosort)))
           (dolist (source-file file-list)
             (let ((target-el-file
                    (expand-file-name (file-name-nondirectory source-file) pkg-dir)))
               (copy-file source-file target-el-file t)))
           ;; Now that the files have been installed, this package is
           ;; indistinguishable from a `tar' or a `single'. Let's make
           ;; things simple by ensuring we're one of them.
           (setf (package-desc-kind pkg-desc)
                 (if (> (length file-list) 1) 'tar 'single))))
        (`tar
         (make-directory package-user-dir t)
         ;; FIXME: should we delete PKG-DIR if it exists?
         (let* ((default-directory (file-name-as-directory package-user-dir)))
           (package-untar-buffer dirname)))
        (`single
         (let ((el-file (expand-file-name (format "%s.el" name) pkg-dir)))
           (make-directory pkg-dir t)
           (package--write-file-no-coding el-file)))
        (kind (error "Unknown package kind: %S" kind)))
      (package--make-autoloads-and-stuff pkg-desc pkg-dir)
      ;; Update package-alist.
      (let ((new-desc (package-load-descriptor pkg-dir)))
        ;; FIXME: Check that `new-desc' matches `desc'!
        ;; Activation has to be done before compilation, so that if we're
        ;; upgrading and macros have changed we load the new definitions
        ;; before compiling.
        (package-activate-1 new-desc :reload :deps)
        ;; FIXME: Compilation should be done as a separate, optional, step.
        ;; E.g. for multi-package installs, we should first install all packages
        ;; and then compile them.
        (package--compile new-desc)
        ;; After compilation, load again any files loaded by
        ;; `activate-1', so that we use the byte-compiled definitions.
        (package--load-files-for-activation new-desc :reload))
      pkg-dir))

  )

(provide 'package-fixes)
;;; package-fixes.el ends here
