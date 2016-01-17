;;; gnome-snippet.el --- GNOME-style code generation -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Daiki Ueno <ueno@gnu.org>

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style

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

(require 'gnome-align)

(eval-when-compile
  (require 'subword))

(declare-function subword-forward "subword.el" (&optional arg))

(defvar gnome-snippet-package nil)
(make-variable-buffer-local 'gnome-snippet-package)

(defvar gnome-snippet-class nil)
(make-variable-buffer-local 'gnome-snippet-class)

(defvar gnome-snippet-parent-package nil)
(make-variable-buffer-local 'gnome-snippet-parent-package)

(defvar gnome-snippet-parent-class nil)
(make-variable-buffer-local 'gnome-snippet-parent-class)

(defcustom gnome-snippet-align-arglist t
  "Whether to align argument list of the inserted snippet"
  :type 'boolean
  :group 'gnome-minor-mode)

(make-variable-buffer-local 'gnome-snippet-align-arglist)

(defun gnome-snippet--parse-name (name)
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

(defun gnome-snippet--read-package-and-class (package-prompt
						class-prompt
						package-symbol
						class-symbol)
  (when (or current-prefix-arg
	    (not (and (symbol-value package-symbol)
		      (symbol-value class-symbol))))
    (set package-symbol
	 (gnome-snippet--parse-name
	  (read-string (or package-prompt
			   "Package (CamelCase): ")
		       (if (symbol-value package-symbol)
			   (gnome-snippet--format-Package
			    (symbol-value package-symbol))))))
    (set class-symbol
	 (gnome-snippet--parse-name
	  (read-string (or class-prompt
			   "Class (CamelCase): ")
		       (if (symbol-value class-symbol)
			   (gnome-snippet--format-Class
			    (symbol-value class-symbol)))))))
  (list (symbol-value package-symbol) (symbol-value class-symbol)))

(defun gnome-snippet--format-PACKAGE (package)
  (mapconcat #'upcase package "_"))
(defalias 'gnome-snippet--format-CLASS 'gnome-snippet--format-PACKAGE)

(defun gnome-snippet--format-PACKAGE_CLASS (package class)
  (concat (gnome-snippet--format-PACKAGE package)
	  "_"
	  (gnome-snippet--format-CLASS class)))

(defun gnome-snippet--format-package (package)
  (mapconcat #'downcase package "_"))
(defalias 'gnome-snippet--format-class 'gnome-snippet--format-package)

(defun gnome-snippet--format-package_class (package class)
  (concat (gnome-snippet--format-package package)
	  "_"
	  (gnome-snippet--format-class class)))

(defun gnome-snippet--format-Package (package)
  (mapconcat #'identity package ""))
(defalias 'gnome-snippet--format-Class 'gnome-snippet--format-Package)

(defun gnome-snippet--format-PackageClass (package class)
  (concat (gnome-snippet--format-Package package)
	  (gnome-snippet--format-Class class)))

;;;###autoload
(defun gnome-snippet-insert-package_class (package class)
  "Insert the class name before the current point."
  (interactive (gnome-snippet--read-package-and-class
		nil nil
		'gnome-snippet-package
		'gnome-snippet-class))
  (insert (gnome-snippet--format-package_class package class)))

;;;###autoload
(defun gnome-snippet-insert-PACKAGE_CLASS (package class)
  "Insert the class name before the current point."
  (interactive (gnome-snippet--read-package-and-class
		nil nil
		'gnome-snippet-package
		'gnome-snippet-class))
  (insert (gnome-snippet--format-PACKAGE_CLASS package class)))

;;;###autoload
(defun gnome-snippet-insert-PackageClass (package class)
  "Insert the class name (in CamelCase) before the current point."
  (interactive (gnome-snippet--read-package-and-class
		nil nil
		'gnome-snippet-package
		'gnome-snippet-class))
  (insert (gnome-snippet--format-PackageClass package class)))

(defun gnome-snippet-insert-interface-declaration (package iface
							     parent-package parent-class)
  "Insert interface declaration for PACKAGE and IFACE"
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (insert "\
#define " (gnome-snippet--format-PACKAGE package) "_TYPE_" (gnome-snippet--format-CLASS iface) " (" (gnome-snippet--format-package package) "_" (gnome-snippet--format-class iface) "_get_type ())
G_DECLARE_INTERFACE (" (gnome-snippet--format-PackageClass package iface) ", "
(gnome-snippet--format-package_class package iface) ", " (gnome-snippet--format-PACKAGE package) ", " (gnome-snippet--format-CLASS iface) ", " (gnome-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gnome-snippet--insert-class-declaration (package
						  class
						  parent-package
						  parent-class
						  derivable)
  (insert "\
#define " (gnome-snippet--format-PACKAGE package) "_TYPE_" (gnome-snippet--format-CLASS class) " (" (gnome-snippet--format-package_class package class) "_get_type ())
G_DECLARE_" (if derivable "DERIVABLE" "FINAL") "_TYPE (" (gnome-snippet--format-PackageClass package class) ", "
(gnome-snippet--format-package_class package class) ", " (gnome-snippet--format-PACKAGE package) ", " (gnome-snippet--format-CLASS class) ", " (gnome-snippet--format-PackageClass parent-package parent-class) ")
"))

(defun gnome-snippet-insert-final-class-declaration (package
						     class
						     parent-package
						     parent-class)
  "Insert final class declaration for PACKAGE and CLASS."
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil nil
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (gnome-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     nil))

(defun gnome-snippet-insert-derivable-class-declaration (package
							 class
							 parent-package
							 parent-class)
  "Insert derivable class declaration for PACKAGE and CLASS."
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil nil
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (gnome-snippet--insert-class-declaration package
					     class
					     parent-package
					     parent-class
					     t))

(defun gnome-snippet-insert-interface-definition (package
						  iface
						  parent-package
						  parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil
	    "Interface (CamelCase): "
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (insert "\
static void
" (gnome-snippet--format-package_class package iface) "_default_init (" (gnome-snippet--format-PackageClass package iface) "Interface *iface) {
}

G_DEFINE_INTERFACE (" (gnome-snippet--format-PackageClass package iface) ", "
(gnome-snippet--format-package_class package iface) ", " (gnome-snippet--format-PACKAGE parent-package) "_TYPE_" (gnome-snippet--format-CLASS parent-class) ")
"))

(defun gnome-snippet--insert-class-definition (package
					       class
					       parent-package
					       parent-class
					       abstract)
  (insert "\
G_DEFINE_" (if abstract "ABSTRACT_" "") "TYPE (" (gnome-snippet--format-PackageClass package class) ", "
(gnome-snippet--format-package_class package class) ", " (gnome-snippet--format-PACKAGE parent-package) "_TYPE_" (gnome-snippet--format-CLASS parent-class) ")

static void
" (gnome-snippet--format-package_class package class) "_class_init (" (gnome-snippet--format-PackageClass package class) "Class *klass)
{
}

static void
" (gnome-snippet--format-package_class package class) "_init (" (gnome-snippet--format-PackageClass package class) " *self)
{
}
"))

(defun gnome-snippet-insert-class-definition (package
					      class
					      parent-package
					      parent-class)
  "Insert class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil nil
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (gnome-snippet--insert-class-definition package
					  class
					  parent-package
					  parent-class
					  nil))

(defun gnome-snippet-insert-abstract-class-definition (package
						       class
						       parent-package
						       parent-class)
  "Insert abstract class definition for PACKAGE and CLASS."
  (interactive
   (append (gnome-snippet--read-package-and-class
	    nil nil
	    'gnome-snippet-package
	    'gnome-snippet-class)
	   (gnome-snippet--read-package-and-class
	    "Parent package (CamelCase): "
	    "Parent class (CamelCase): "
	    'gnome-snippet-parent-package
	    'gnome-snippet-parent-class)))
  (gnome-snippet--insert-class-definition package
					  class
					  parent-package
					  parent-class
					  t))

(defun gnome-snippet-insert-constructor (package class)
  "Insert 'constructor' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static GObject *
" (gnome-snippet--format-package_class package class) "_constructor (")
    (setq arglist-start (point-marker))
    (insert "GType *object,
guint n_construct_properties,
GObjectConstructParam *construct_properties)\n")
    (setq body-start (point-marker))
    (if gnome-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-align-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-snippet--format-package_class package class) "_parent_class)->constructed (object);
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-set_property (package class)
  "Insert 'set_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_set_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
const GValue *value,
GParamSpec *pspec)\n")
    (setq body-start (point-marker))
    (if gnome-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-align-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-get_property (package class)
  "Insert 'get_property' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (arglist-start body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_get_property (")
    (setq arglist-start (point-marker))
    (insert "GObject *object,
guint prop_id,
GValue *value,
GParamSpec *pspec)\n")
    (setq body-start (point-marker))
    (if gnome-snippet-align-arglist
	(progn
	  (goto-char arglist-start)
	  (gnome-align-at-point))
      (indent-region arglist-start (point)))
    (goto-char body-start)
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
(gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  switch (prop_id)
    {
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
      break;
    }
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-dispose (package class)
  "Insert 'dispose' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_dispose (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-snippet--format-package_class package class) "_parent_class)->dispose (object);
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-finalize (package class)
  "Insert 'finalize' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_finalize (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-snippet--format-package_class package class) "_parent_class)->finalize (object);
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-notify (package class)
  "Insert 'notify' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_notify (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-snippet--format-package_class package class) "_parent_class)->finalize (object);
}
")
    (indent-region body-start (point))))

(defun gnome-snippet-insert-constructed (package class)
  "Insert 'constructed' vfunc of GObjectClass for PACKAGE and CLASS."
  (interactive
   (gnome-snippet--read-package-and-class
    nil nil
    'gnome-snippet-package
    'gnome-snippet-class))
  (let (body-start)
    (insert "\
static void
" (gnome-snippet--format-package_class package class) "_constructed (GObject *object)\n")
    (setq body-start (point-marker))
    (insert "{
  " (gnome-snippet--format-PackageClass package class) " *self = "
  (gnome-snippet--format-PACKAGE_CLASS package class) " (object);

  G_OBJECT_CLASS (" (gnome-snippet--format-package_class package class) "_parent_class)->constructed (object);
}
")
    (indent-region body-start (point))))

(defvar gnome-snippet-snippet-commands
  '(("G_DECLARE_INTERFACE" . gnome-snippet-insert-interface-declaration)
    ("G_DECLARE_FINAL_TYPE" . gnome-snippet-insert-final-class-declaration)
    ("G_DECLARE_DERIVABLE_TYPE" .
     gnome-snippet-insert-derivable-class-declaration)
    ("G_DEFINE_INTERFACE" . gnome-snippet-insert-interface-definition)
    ("G_DEFINE_TYPE" . gnome-snippet-insert-class-definition)
    ("G_DEFINE_ABSTRACT_TYPE" .
     gnome-snippet-insert-abstract-class-definition)
    ("GObjectClass.constructor" . gnome-snippet-insert-constructor)
    ("GObjectClass.set_property" . gnome-snippet-insert-set_property)
    ("GObjectClass.get_property" . gnome-snippet-insert-get_property)
    ("GObjectClass.dispose" . gnome-snippet-insert-dispose)
    ("GObjectClass.finalize" . gnome-snippet-insert-finalize)
    ;; GObjectClass.dispatch_properties_changed
    ("GObjectClass.notify" . gnome-snippet-insert-notify)
    ("GObjectClass.contructed" . gnome-snippet-insert-constructed)))

;;;###autoload
(defun gnome-snippet-insert (snippet)
  (interactive
   (list (completing-read "Snippet: " gnome-snippet-snippet-commands nil t)))
  (let ((entry (assoc snippet gnome-snippet-snippet-commands)))
    (unless entry
      (error "Unknown snippet: %s" snippet))
    (call-interactively (cdr entry))))

(provide 'gnome-snippet)

;;; gnome-snippet.el ends here
