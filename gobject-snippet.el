;;; gobject-snippet.el --- GObject code generation -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Daiki Ueno <ueno@gnu.org>

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GObject, C, coding style

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Code:

(require 'gobject-align)

(eval-when-compile
  (require 'subword))

(declare-function subword-forward "subword.el" (&optional arg))

(defvar gobject-snippet-package nil)
(make-variable-buffer-local 'gobject-snippet-package)

(defvar gobject-snippet-class nil)
(make-variable-buffer-local 'gobject-snippet-class)

(defvar gobject-snippet-parent-package nil)
(make-variable-buffer-local 'gobject-snippet-parent-package)

(defvar gobject-snippet-parent-class nil)
(make-variable-buffer-local 'gobject-snippet-parent-class)

(defvar gobject-snippet-align-arglist nil)
(make-variable-buffer-local 'gobject-snippet-align-arglist)

(defun gobject-snippet--parse-name (name)
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

(defun gobject-snippet--read-package-and-class (package-prompt
						class-prompt
						package-symbol
						class-symbol)
  (when (or current-prefix-arg
	    (not (and (symbol-value package-symbol)
		      (symbol-value class-symbol))))
    (set package-symbol
	 (gobject-snippet--parse-name
	  (read-string (or package-prompt
			   "Package (CamelCase): ")
		       (if (symbol-value package-symbol)
			   (gobject-snippet--format-Package
			    (symbol-value package-symbol))))))
    (set class-symbol
	 (gobject-snippet--parse-name
	  (read-string (or class-prompt
			   "Class (CamelCase): ")
		       (if (symbol-value class-symbol)
			   (gobject-snippet--format-Class
			    (symbol-value class-symbol)))))))
  (list (symbol-value package-symbol) (symbol-value class-symbol)))

(defun gobject-snippet--format-PACKAGE (package)
  (mapconcat #'upcase package "_"))
(defalias 'gobject-snippet--format-CLASS 'gobject-snippet--format-PACKAGE)

(defun gobject-snippet--format-PACKAGE_CLASS (package class)
  (concat (gobject-snippet--format-PACKAGE package)
	  "_"
	  (gobject-snippet--format-CLASS class)))

(defun gobject-snippet--format-package (package)
  (mapconcat #'downcase package "_"))
(defalias 'gobject-snippet--format-class 'gobject-snippet--format-package)

(defun gobject-snippet--format-package_class (package class)
  (concat (gobject-snippet--format-package package)
	  "_"
	  (gobject-snippet--format-class class)))

(defun gobject-snippet--format-Package (package)
  (mapconcat #'identity package ""))
(defalias 'gobject-snippet--format-Class 'gobject-snippet--format-Package)

(defun gobject-snippet--format-PackageClass (package class)
  (concat (gobject-snippet--format-Package package)
	  (gobject-snippet--format-Class class)))

;;;###autoload
(defun gobject-snippet-insert-package_class (package class)
  "Insert the class name before the current point."
  (interactive (gobject-snippet--read-package-and-class
		nil nil
		'gobject-snippet-package
		'gobject-snippet-class))
  (insert (gobject-snippet--format-package_class package class)))

;;;###autoload
(defun gobject-snippet-insert-PACKAGE_CLASS (package class)
  "Insert the class name before the current point."
  (interactive (gobject-snippet--read-package-and-class
		nil nil
		'gobject-snippet-package
		'gobject-snippet-class))
  (insert (gobject-snippet--format-PACKAGE_CLASS package class)))

;;;###autoload
(defun gobject-snippet-insert-PackageClass (package class)
  "Insert the class name (in CamelCase) before the current point."
  (interactive (gobject-snippet--read-package-and-class
		nil nil
		'gobject-snippet-package
		'gobject-snippet-class))
  (insert (gobject-snippet--format-PackageClass package class)))

(defun gobject-snippet-insert-interface-declaration (package iface
							     parent-package parent-class)
  "Insert interface declaration for PACKAGE and IFACE"
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (insert "\
#define " (gobject-snippet--format-PACKAGE package) "_TYPE_" (gobject-snippet--format-CLASS iface) " (" (gobject-snippet--format-package package) "_" (gobject-snippet--format-class iface) "_get_type ())
G_DECLARE_INTERFACE (" (gobject-snippet--format-PackageClass package iface) ", "
(gobject-snippet--format-package_class package iface) ", " (gobject-snippet--format-PACKAGE package) ", " (gobject-snippet--format-CLASS iface) ", " (gobject-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gobject-snippet--insert-class-declaration (package
						  class
						  parent-package
						  parent-class
						  derivable)
  (insert "\
#define " (gobject-snippet--format-PACKAGE package) "_TYPE_" (gobject-snippet--format-CLASS class) " (" (gobject-snippet--format-package_class package class) "_get_type ())
G_DECLARE_" (if derivable "DERIVABLE" "FINAL") "_TYPE (" (gobject-snippet--format-PackageClass package class) ", "
(gobject-snippet--format-package_class package class) ", " (gobject-snippet--format-PACKAGE package) ", " (gobject-snippet--format-CLASS class) ", " (gobject-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gobject-snippet-insert-final-class-declaration (package
						     class
						     parent-package
						     parent-class)
  "Insert final class declaration for PACKAGE and CLASS."
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil nil
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (gobject-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     nil))

(defun gobject-snippet-insert-derivable-class-declaration (package
							 class
							 parent-package
							 parent-class)
  "Insert derivable class declaration for PACKAGE and CLASS."
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil nil
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (gobject-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     t))

(defun gobject-snippet-insert-interface-definition (package
						  iface
						  parent-package
						  parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (insert "\
static void
" (gobject-snippet--format-package_class package iface) "_default_init (" (gobject-snippet--format-PackageClass package iface) "Interface *iface) {
}

G_DEFINE_INTERFACE (" (gobject-snippet--format-PackageClass package iface) ", "
(gobject-snippet--format-package_class package iface) ", " (gobject-snippet--format-PACKAGE parent-package) "_TYPE_" (gobject-snippet--format-CLASS parent-class) ")
"))

(defun gobject-snippet--insert-class-definition (package
					       class
					       parent-package
					       parent-class
					       abstract)
  (insert "\
G_DEFINE_" (if abstract "ABSTRACT_" "") "TYPE (" (gobject-snippet--format-PackageClass package class) ", "
(gobject-snippet--format-package_class package class) ", " (gobject-snippet--format-PACKAGE parent-package) "_TYPE_" (gobject-snippet--format-CLASS parent-class) ")

static void
" (gobject-snippet--format-package_class package class) "_class_init (" (gobject-snippet--format-PackageClass package class) "Class *klass)
{
}

static void
" (gobject-snippet--format-package_class package class) "_init (" (gobject-snippet--format-PackageClass package class) " *self)
{
}
"))

(defun gobject-snippet-insert-class-definition (package
					      class
					      parent-package
					      parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil nil
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (gobject-snippet--insert-class-definition package
					  class
					  parent-package
					  parent-class
					  nil))

(defun gobject-snippet-insert-abstract-class-definition (package
						       class
						       parent-package
						       parent-class)
  "Insert abstract class definition for PACKAGE and CLASS."
  (interactive
   (append (gobject-snippet--read-package-and-class
	    nil nil
	    'gobject-snippet-package
	    'gobject-snippet-class)
	   (gobject-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gobject-snippet-parent-package
	    'gobject-snippet-parent-class)))
  (gobject-snippet--insert-class-definition package
					  class
					  parent-package
					  parent-class
					  t))

(defun gobject-snippet-insert-constructor (package class)
  "Insert 'constructor' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static GObject *
" (gobject-snippet--format-package_class package class) "_constructor (")
    (setq arglist-start (point-marker))
    (insert "GType *object,
guint n_construct_properties,
GObjectConstructParam *construct_properties")
    (funcall (if gobject-snippet-align-arglist
		 #'gobject-align-region
	       #'indent-region)
	     arglist-start (point))
    (insert ")\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gobject-snippet--format-package_class package class) "_parent_class)->constructed (object);
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-set_property (package class)
  "Insert 'set_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_set_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
const GValue *value,
GParamSpec *pspec")
    (funcall (if gobject-snippet-align-arglist
		 #'gobject-align-region
	       #'indent-region)
	     arglist-start (point))
    (insert ")\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-get_property (package class)
  "Insert 'get_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_get_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
GValue *value,
GParamSpec *pspec")
    (funcall (if gobject-snippet-align-arglist
		 #'gobject-align-region
	       #'indent-region)
	     arglist-start (point))
    (insert ")\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
(gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-dispose (package class)
  "Insert 'dispose' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_dispose (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gobject-snippet--format-package_class package class) "_parent_class)->dispose (object);
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-finalize (package class)
  "Insert 'finalize' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_finalize (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gobject-snippet--format-package_class package class) "_parent_class)->finalize (object);
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-notify (package class)
  "Insert 'notify' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_notify (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gobject-snippet--format-package_class package class) "_parent_class)->finalize (object);
}
")
    (indent-region body-start (point))))

(defun gobject-snippet-insert-constructed (package class)
  "Insert 'constructed' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gobject-snippet--read-package-and-class
    nil nil
    'gobject-snippet-package
    'gobject-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gobject-snippet--format-package_class package class) "_constructed (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gobject-snippet--format-PackageClass package class) " *self = "
  (gobject-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gobject-snippet--format-package_class package class) "_parent_class)->constructed (object);
}
")
    (indent-region body-start (point))))

(defvar gobject-snippet-snippet-commands
  '(("G_DECLARE_INTERFACE" . gobject-snippet-insert-interface-declaration)
    ("G_DECLARE_FINAL_TYPE" . gobject-snippet-insert-final-class-declaration)
    ("G_DECLARE_DERIVABLE_TYPE" .
     gobject-snippet-insert-derivable-class-declaration)
    ("G_DEFINE_INTERFACE" . gobject-snippet-insert-interface-definition)
    ("G_DEFINE_TYPE" . gobject-snippet-insert-class-definition)
    ("G_DEFINE_ABSTRACT_TYPE" .
     gobject-snippet-insert-abstract-class-definition)
    ("GObjectClass.constructor" . gobject-snippet-insert-constructor)
    ("GObjectClass.set_property" . gobject-snippet-insert-set_property)
    ("GObjectClass.get_property" . gobject-snippet-insert-get_property)
    ("GObjectClass.dispose" . gobject-snippet-insert-dispose)
    ("GObjectClass.finalize" . gobject-snippet-insert-finalize)
    ;; GObjectClass.dispatch_properties_changed
    ("GObjectClass.notify" . gobject-snippet-insert-notify)
    ("GObjectClass.contructed" . gobject-snippet-insert-constructed)))

;;;###autoload
(defun gobject-snippet-insert (snippet)
  (interactive
   (list (completing-read "Snippet: " gobject-snippet-snippet-commands nil t)))
  (let ((entry (assoc snippet gobject-snippet-snippet-commands)))
    (unless entry
      (error "Unknown snippet: %s" snippet))
    (call-interactively (cdr entry))))

(provide 'gobject-snippet)

;;; gobject-snippet.el ends here
