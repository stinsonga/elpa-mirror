;;; gnome-c-snippet.el --- GNOME-style code generation -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style

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

;; FIXME: The snippets defined here could be rewritten in yasnippet

;;; Code:

(require 'gnome-c-align)

(eval-when-compile
  (require 'subword))

(declare-function subword-forward "subword.el" (&optional arg))

(defvar gnome-c-snippet-package nil)
(make-variable-buffer-local 'gnome-c-snippet-package)

(defvar gnome-c-snippet-class nil)
(make-variable-buffer-local 'gnome-c-snippet-class)

(defvar gnome-c-snippet-parent-package nil)
(make-variable-buffer-local 'gnome-c-snippet-parent-package)

(defvar gnome-c-snippet-parent-class nil)
(make-variable-buffer-local 'gnome-c-snippet-parent-class)

(defcustom gnome-c-snippet-align-arglist t
  "Whether to align argument list of the inserted snippet"
  :type 'boolean
  :group 'gnome-c-style)

(make-variable-buffer-local 'gnome-c-snippet-align-arglist)

(defun gnome-c-snippet--parse-name (name)
  (require 'subword)
  (with-temp-buffer
    (let (words)
      (insert name)
      (goto-char (point-min))
      (while (not (eobp))
	;; Skip characters not recognized by subword-mode.
	(if (looking-at "[^[:lower:][:upper:][:digit:]]+")
	    (goto-char (match-end 0)))
	(push (buffer-substring (point) (progn (subword-forward 1)
					       (point)))
	      words))
      (nreverse words))))

(defun gnome-c-snippet--read-package-and-class (package-prompt
						class-prompt
						package-symbol
						class-symbol)
  (when (or current-prefix-arg
	    (not (and (symbol-value package-symbol)
		      (symbol-value class-symbol))))
    (set package-symbol
	 (gnome-c-snippet--parse-name
	  (read-string (or package-prompt
			   "Package (CamelCase): ")
		       (if (symbol-value package-symbol)
			   (gnome-c-snippet--format-Package
			    (symbol-value package-symbol))))))
    (set class-symbol
	 (gnome-c-snippet--parse-name
	  (read-string (or class-prompt
			   "Class (CamelCase): ")
		       (if (symbol-value class-symbol)
			   (gnome-c-snippet--format-Class
			    (symbol-value class-symbol)))))))
  (list (symbol-value package-symbol) (symbol-value class-symbol)))

(defun gnome-c-snippet--format-PACKAGE (package)
  (mapconcat #'upcase package "_"))
(defalias 'gnome-c-snippet--format-CLASS 'gnome-c-snippet--format-PACKAGE)

(defun gnome-c-snippet--format-PACKAGE_CLASS (package class)
  (concat (gnome-c-snippet--format-PACKAGE package)
	  "_"
	  (gnome-c-snippet--format-CLASS class)))

(defun gnome-c-snippet--format-package (package)
  (mapconcat #'downcase package "_"))
(defalias 'gnome-c-snippet--format-class 'gnome-c-snippet--format-package)

(defun gnome-c-snippet--format-package_class (package class)
  (concat (gnome-c-snippet--format-package package)
	  "_"
	  (gnome-c-snippet--format-class class)))

(defun gnome-c-snippet--format-Package (package)
  (mapconcat #'identity package ""))
(defalias 'gnome-c-snippet--format-Class 'gnome-c-snippet--format-Package)

(defun gnome-c-snippet--format-PackageClass (package class)
  (concat (gnome-c-snippet--format-Package package)
	  (gnome-c-snippet--format-Class class)))

;;;###autoload
(defun gnome-c-snippet-insert-package_class (package class)
  "Insert the class name before the current point."
  (interactive (gnome-c-snippet--read-package-and-class
		nil nil
		'gnome-c-snippet-package
		'gnome-c-snippet-class))
  (insert (gnome-c-snippet--format-package_class package class)))

;;;###autoload
(defun gnome-c-snippet-insert-PACKAGE_CLASS (package class)
  "Insert the class name before the current point."
  (interactive (gnome-c-snippet--read-package-and-class
		nil nil
		'gnome-c-snippet-package
		'gnome-c-snippet-class))
  (insert (gnome-c-snippet--format-PACKAGE_CLASS package class)))

;;;###autoload
(defun gnome-c-snippet-insert-PackageClass (package class)
  "Insert the class name (in CamelCase) before the current point."
  (interactive (gnome-c-snippet--read-package-and-class
		nil nil
		'gnome-c-snippet-package
		'gnome-c-snippet-class))
  (insert (gnome-c-snippet--format-PackageClass package class)))

(defun gnome-c-snippet-insert-interface-declaration (package iface
							     parent-package parent-class)
  "Insert interface declaration for PACKAGE and IFACE"
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (insert "\
#define " (gnome-c-snippet--format-PACKAGE package) "_TYPE_" (gnome-c-snippet--format-CLASS iface) " (" (gnome-c-snippet--format-package package) "_" (gnome-c-snippet--format-class iface) "_get_type ())
G_DECLARE_INTERFACE (" (gnome-c-snippet--format-PackageClass package iface) ", "
(gnome-c-snippet--format-package_class package iface) ", " (gnome-c-snippet--format-PACKAGE package) ", " (gnome-c-snippet--format-CLASS iface) ", " (gnome-c-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gnome-c-snippet--insert-class-declaration (package
						  class
						  parent-package
						  parent-class
						  derivable)
  (insert "\
#define " (gnome-c-snippet--format-PACKAGE package) "_TYPE_" (gnome-c-snippet--format-CLASS class) " (" (gnome-c-snippet--format-package_class package class) "_get_type ())
G_DECLARE_" (if derivable "DERIVABLE" "FINAL") "_TYPE (" (gnome-c-snippet--format-PackageClass package class) ", "
(gnome-c-snippet--format-package_class package class) ", " (gnome-c-snippet--format-PACKAGE package) ", " (gnome-c-snippet--format-CLASS class) ", " (gnome-c-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gnome-c-snippet-insert-final-class-declaration (package
						       class
						       parent-package
						       parent-class)
  "Insert final class declaration for PACKAGE and CLASS."
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil nil
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (gnome-c-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     nil))

(defun gnome-c-snippet-insert-derivable-class-declaration (package
							   class
							   parent-package
							   parent-class)
  "Insert derivable class declaration for PACKAGE and CLASS."
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil nil
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (gnome-c-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     t))

(defun gnome-c-snippet-insert-interface-definition (package
						    iface
						    parent-package
						    parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (insert "\
static void
" (gnome-c-snippet--format-package_class package iface) "_default_init (" (gnome-c-snippet--format-PackageClass package iface) "Interface *iface) {
}

G_DEFINE_INTERFACE (" (gnome-c-snippet--format-PackageClass package iface) ", "
(gnome-c-snippet--format-package_class package iface) ", " (gnome-c-snippet--format-PACKAGE parent-package) "_TYPE_" (gnome-c-snippet--format-CLASS parent-class) ")
"))

(defun gnome-c-snippet--insert-class-definition (package
						 class
						 parent-package
						 parent-class
						 abstract)
  (insert "\
G_DEFINE_" (if abstract "ABSTRACT_" "") "TYPE (" (gnome-c-snippet--format-PackageClass package class) ", "
(gnome-c-snippet--format-package_class package class) ", " (gnome-c-snippet--format-PACKAGE parent-package) "_TYPE_" (gnome-c-snippet--format-CLASS parent-class) ")

static void
" (gnome-c-snippet--format-package_class package class) "_class_init (" (gnome-c-snippet--format-PackageClass package class) "Class *klass)
{
}

static void
" (gnome-c-snippet--format-package_class package class) "_init (" (gnome-c-snippet--format-PackageClass package class) " *self)
{
}
"))

(defun gnome-c-snippet-insert-class-definition (package
						class
						parent-package
						parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil nil
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (gnome-c-snippet--insert-class-definition package
					    class
					    parent-package
					    parent-class
					    nil))

(defun gnome-c-snippet-insert-abstract-class-definition (package
							 class
							 parent-package
							 parent-class)
  "Insert abstract class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-c-snippet--read-package-and-class
	    nil nil
	    'gnome-c-snippet-package
	    'gnome-c-snippet-class)
	   (gnome-c-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-c-snippet-parent-package
	    'gnome-c-snippet-parent-class)))
  (gnome-c-snippet--insert-class-definition package
					    class
					    parent-package
					    parent-class
					    t))

(defun gnome-c-snippet-insert-constructor (package class)
  "Insert 'constructor' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static GObject *
" (gnome-c-snippet--format-package_class package class) "_constructor (")
    (setq arglist-start (point-marker))
    (insert "GType *object,
guint n_construct_properties,
GObjectConstructParam *construct_properties)\n")
    (setq body-start (point-marker))
    (if gnome-c-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-c-align-arglist-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->constructed (type, n_construct_properties, construct_properties);
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-set_property (package class)
  "Insert 'set_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_set_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
const GValue *value,
GParamSpec *pspec)\n")
    (setq body-start (point-marker))
    (if gnome-c-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-c-align-arglist-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-get_property (package class)
  "Insert 'get_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_get_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
GValue *value,
GParamSpec *pspec)\n")
    (setq body-start (point-marker))
    (if gnome-c-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-c-align-arglist-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-dispose (package class)
  "Insert 'dispose' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_dispose (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->dispose (object);
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-finalize (package class)
  "Insert 'finalize' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_finalize (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->finalize (object);
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-dispatch_properties_changed (package class)
  "Insert 'dispatch_properties_changed vfunc of GObjectClass for
PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_dispatch_properties_changed (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint n_pspecs,
GParamSpec **pspecs)\n")
    (setq body-start (point-marker))
    (if gnome-c-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-c-align-arglist-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->dispatch_properties_changed (object, n_pspecs, pspecs);
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-notify (package class)
  "Insert 'notify' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_notify (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
GParamSpec *pspec)\n")
    (setq body-start (point-marker))
    (if gnome-c-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-c-align-arglist-at-point))
      (indent-region arglist-start (point)))
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->notify (object, pspec);
}
")
    (indent-region body-start (point))))

(defun gnome-c-snippet-insert-constructed (package class)
  "Insert 'constructed' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-c-snippet--read-package-and-class
    nil nil
    'gnome-c-snippet-package
    'gnome-c-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-c-snippet--format-package_class package class) "_constructed (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-c-snippet--format-PackageClass package class) " *self = "
  (gnome-c-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-c-snippet--format-package_class package class) "_parent_class)->constructed (object);
}
")
    (indent-region body-start (point))))

(defvar gnome-c-snippet-snippet-commands
  '(("G_DECLARE_INTERFACE" . gnome-c-snippet-insert-interface-declaration)
    ("G_DECLARE_FINAL_TYPE" . gnome-c-snippet-insert-final-class-declaration)
    ("G_DECLARE_DERIVABLE_TYPE" .
     gnome-c-snippet-insert-derivable-class-declaration)
    ("G_DEFINE_INTERFACE" . gnome-c-snippet-insert-interface-definition)
    ("G_DEFINE_TYPE" . gnome-c-snippet-insert-class-definition)
    ("G_DEFINE_ABSTRACT_TYPE" .
     gnome-c-snippet-insert-abstract-class-definition)
    ("GObjectClass.constructor" . gnome-c-snippet-insert-constructor)
    ("GObjectClass.set_property" . gnome-c-snippet-insert-set_property)
    ("GObjectClass.get_property" . gnome-c-snippet-insert-get_property)
    ("GObjectClass.dispose" . gnome-c-snippet-insert-dispose)
    ("GObjectClass.finalize" . gnome-c-snippet-insert-finalize)
    ("GObjectClass.dispatch_properties_changed" .
     gnome-c-snippet-insert-dispatch_properties_changed)
    ("GObjectClass.notify" . gnome-c-snippet-insert-notify)
    ("GObjectClass.contructed" . gnome-c-snippet-insert-constructed)))

;;;###autoload
(defun gnome-c-snippet-insert (snippet)
  (interactive
   (list (completing-read "Snippet: " gnome-c-snippet-snippet-commands nil t)))
  (let ((entry (assoc snippet gnome-c-snippet-snippet-commands)))
    (unless entry
      (error "Unknown snippet: %s" snippet))
    (call-interactively (cdr entry))))

(provide 'gnome-c-snippet)

;;; gnome-c-snippet.el ends here
