;;;; build-gtk.jl -- translate guile-gtk .defs file to rep C code
;;;  Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;;;  $Id$
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this software; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(provide 'build-gtk)

(setq debug-on-error '(bad-arg invalid-function missing-arg))

;; Notes:

;; This assumes that the `sed-fix-defs' sed script has been run over all
;; input files (to convert schemey things to their lispy equivalents)

;; Todo:
;;  * doesn't check for `listable' type-property
;;  * guile-gtk `struct' and `ptype' types
;;  * not possible to wrap functions returning vector types

;; WARNING: This makes some pretty gruesome assumptions. [where?]


;; Configuration

;; Alist of (TYPE ["C-TYPE" | DECL-FUNC] ["REP2GTK" | FROM-REP-FUNC]
;;           ["GTK2REP" | TO-REP-FUNC] ["PRED-NAME" | PRED-FUNC]
;;	     . OPTION-ALIST)

;; The required functions are called as:

;;   (DECL-FUNC TYPE TYPE-INFO)
;;   (FROM-REP-FUNC OUTPUT-STREAM TYPE "REP-VAR" TYPE-INFO OPTIONS)
;;   (TO-REP-FUNC OUTPUT-STREAM TYPE "GTK-VAR" TYPE-INFO OPTIONS)
;;   (PRED-FUNC OUTPUT-STREAM TYPE "REP-VAR" TYPE-INFO OPTIONS)

;; The options in the OPTION-ALIST may be:

;;   (c2args . EMIT-ARG-FUNC)
;;   (finish . FINISH-ARG-FUNC)
;;   (listable . BOOLEAN)

;; with:

;;   (EMIT-ARG-FUNC OUTPUT TYPE "GTK-VAR" OPTIONS)
;;   (FINISH-ARG-FUNC OUTPUT TYPE "GTK-VAR" "REP-VAR" OPTIONS)

(defvar gtk-type-alist nil)

(defun define-type (type c-type rep-to-gtk gtk-to-rep type-pred . options)
  (setq gtk-type-alist (cons (list* type c-type rep-to-gtk
				    gtk-to-rep type-pred options)
			     gtk-type-alist)))


;; Work variables

(defvar gtk-enums nil
  "List of (ENUM-NAME . ENUM-DEF) for all parsed enums defs")

(defvar gtk-string-enums nil
  "List of (ENUM-NAME . ENUM-DEF) for all parsed enums defs")

(defvar gtk-flags nil
  "List of (ENUM-NAME . ENUM-DEF) for all parsed flags defs")

(defvar gtk-boxed nil
  "List of (BOXED-NAME . BOXED-DEF)")

(defvar gtk-objects nil
  "List of (OBJECT-NAME . OBJECT-DEF)")

(defvar gtk-functions nil
  "List of (FUNCTION-NAME . FUNCTION-DEF)")

(defvar gtk-options nil
  "List of (OPTION VALUE)")

(defvar gtk-subrs nil
  "List of C-NAME.")

;; similar for imported files
(defvar gtk-imported-enums nil)
(defvar gtk-imported-string-enums nil)
(defvar gtk-imported-flags nil)
(defvar gtk-imported-boxed nil)
(defvar gtk-imported-objects nil)

;; t when importing secondary definitions
(defvar gtk-importing nil)

(defmacro gtk-get-options (name options)
  `(cdr (assq ,name ,options)))

(defmacro gtk-get-option (name options)
  `(car (gtk-get-options ,name ,options)))

(defvar gtk-hyphen-map
  (let
      ((map (make-string (1+ ?_)))
       (i 0))
    (while (< i ?_)
      (aset map i i)
      (setq i (1+ i)))
    (aset map i ?-)
    map))

(defvar gtk-unhyphen-map
  (let
      ((map (make-string (1+ ?-)))
       (i 0))
    (while (< i ?-)
      (aset map i i)
      (setq i (1+ i)))
    (aset map i ?_)
    map))

(defvar gtk-emitted-composite-helpers nil)


;; Entry point

(defun build-gtk (defs-file-name output-file-name)
  (let
      ((gtk-enums nil)
       (gtk-string-enums nil)
       (gtk-flags nil)
       (gtk-boxed nil)
       (gtk-objects nil)
       (gtk-functions nil)
       (gtk-options nil)
       (gtk-subrs nil)
       (gtk-imported-enums nil)
       (gtk-imported-string-enums nil)
       (gtk-imported-flags nil)
       (gtk-imported-boxed nil)
       (gtk-imported-objects nil)
       (gtk-importing nil)
       (gtk-emitted-composite-helpers nil))
    (let
	((defs-file (open-file defs-file-name 'read)))
      (or defs-file (error "Can't open input file: %s" defs-file-name))
      (unwind-protect
	  (parse-gtk defs-file)
	(close-file defs-file)))
    (setq gtk-enums (nreverse gtk-enums))
    (setq gtk-string-enums (nreverse gtk-string-enums))
    (setq gtk-flags (nreverse gtk-flags))
    (setq gtk-boxed (nreverse gtk-boxed))
    (setq gtk-objects (nreverse gtk-objects))
    (setq gtk-functions (nreverse gtk-functions))
    (let
	((output-file (open-file output-file-name 'write)))
      (or output-file (error "Can't open output file: %s" output-file-name))
      (unwind-protect
	  (let
	      ((standard-output output-file))
	    (output-gtk output-file))
	(close-file output-file)))))

(defun build-gtk-batch ()
  (or (= (length command-line-args) 2) (error "usage: INPUT OUTPUT"))
  (let
      ((in (car command-line-args))
       (out (nth 1 command-line-args)))
    (setq command-line-args (nthcdr 2 command-line-args))
    (build-gtk in out)))


;; Parsing

(defun parse-gtk (input)
  (condition-case nil
      (while t
	(let
	    ((def (read input)))
	  ;;(format standard-error "read: %S\n" def)
	  (when def
	    (or (consp def) (error "Definition isn't a list"))
	    (cond
	     ((memq (car def) '(include import))
	      (let
		  ((file (open-file (expand-file-name (nth 1 def)
						      (file-name-directory
						       (file-binding input)))
				    'read)))
		(or file (error "Can't open input file: %s" (nth 1 def)))
		(unwind-protect
		    (let ((gtk-importing (if (eq (car def) 'import)
					     t
					   gtk-importing)))
		      (parse-gtk file))
		  (close-file file))))
	     ((eq (car def) 'define-enum)
	      (let*
		  ((name (nth 1 def))
		   (body (nthcdr 2 def))
		   (cell (or (assq name gtk-enums)
			     (assq name gtk-imported-enums))))
		(if cell
		    (rplacd cell body)
		  (if (not gtk-importing)
		      (setq gtk-enums (cons (cons name body) gtk-enums))
		    (setq gtk-imported-enums
			  (cons (cons name body) gtk-imported-enums))))))
	     ((eq (car def) 'define-string-enum)
	      (let*
		  ((name (nth 1 def))
		   (body (nthcdr 2 def))
		   (cell (or (assq name gtk-string-enums)
			     (assq name gtk-imported-string-enums))))
		(if cell
		    (rplacd cell body)
		  (if (not gtk-importing)
		      (setq gtk-string-enums (cons (cons name body)
						   gtk-string-enums))
		    (setq gtk-imported-string-enums
			  (cons (cons name body)
				gtk-imported-string-enums))))))
	     ((eq (car def) 'define-flags)
	      (let*
		  ((name (nth 1 def))
		   (body (nthcdr 2 def))
		   (cell (or (assq name gtk-flags)
			     (assq name gtk-imported-flags))))
		(if cell
		    (rplacd cell body)
		  (if (not gtk-importing)
		      (setq gtk-flags (cons (cons name body) gtk-flags))
		    (setq gtk-imported-flags
			  (cons (cons name body) gtk-imported-flags))))))
	     ((eq (car def) 'define-boxed)
	      (let
		  ((cell (or (assq (nth 1 def) gtk-boxed)
			     (assq (nth 1 def) gtk-imported-boxed))))
		(if cell
		    (rplacd cell (nthcdr 2 def))
		  (if (not gtk-importing)
		      (setq gtk-boxed (cons (cdr def) gtk-boxed))
		    (setq gtk-imported-boxed
			  (cons (cdr def) gtk-imported-boxed))))))
	     ((eq (car def) 'define-object)
	      (let*
		  ((name (nth 1 def))
		   (super (nth 2 def))
		   (attrs (nthcdr 3 def))
		   (cell (or (assq name gtk-objects)
			     (assq name gtk-imported-objects))))
		(when (car super)
		  (setq attrs (cons (cons 'super (car super)) attrs)))
		(if cell
		    (rplacd cell attrs)
		  (if (not gtk-importing)
		      (setq gtk-objects
			    (cons (cons name attrs) gtk-objects))
		    (setq gtk-imported-objects
			  (cons (cons name attrs) gtk-imported-objects))))))
	     ((eq (car def) 'define-func)
	      (unless gtk-importing
		(let
		    ((cell (assq (nth 1 def) gtk-functions)))
		  (if cell
		      (rplacd cell (nthcdr 2 def))
		    (setq gtk-functions (cons (cdr def) gtk-functions))))))
	     ((eq (car def) 'define-type)
	      (eval def))
	     ((eq (car def) 'options)
	      (unless gtk-importing
		(mapc (lambda (cell)
			(let
			    ((value (assq (car cell) gtk-options)))
			  (if value
			      (rplacd value (nconc (cdr value)
						   (list (nth 1 cell))))
			    (setq gtk-options (cons cell gtk-options)))))
		      (cdr def))))
	     ((eq (car def) 'add-options)
	      (unless gtk-importing
		(let
		    ((value (assq (nth 1 def) gtk-options)))
		  (if value
		      (rplacd value (nconc (cdr value) (nthcdr 2 def)))
		    (setq gtk-options (cons (cdr def) gtk-options))))))
	     (t
	      (gtk-warning "Ignoring `%S'" def))))))
    (end-of-stream)))


;; Code generation

(defmacro @ args
  (list* 'format 'output args))

(defun output-header (output)
  (@ "/* Automatically generated by build-gtk, DO NOT EDIT! */\n\n")
  (when (gtk-get-options 'includes gtk-options)
    (mapc (lambda (opt)
	    (@ "%s\n" opt))
	  (gtk-get-options 'includes gtk-options)))
  (@ "#include <rep.h>\n")
  (@ "#include \"rep-gtk.h\"\n\n"))

(defun output-footer (output)
  (let*
      ((feature (gtk-get-option 'provide gtk-options))
       (aliases (gtk-get-options 'alias gtk-options))
       (init (gtk-get-option 'init-func gtk-options)))
    (when feature
      (@ "\nrepv\nrep_dl_init \(void\)\n{\n")
      (@ "  repv s = rep_push_structure \(\"%s\"\);\n" feature)
      (mapc (lambda (a)
	      (@ "  /* ::alias:%s %s:: */\n" a feature)
	      (@ "  rep_alias_structure \(\"%s\"\);\n" a)) aliases)
      (when init
	(@ "\n  %s \(\);\n\n" init))
      (@ "  return rep_pop_structure \(s\);\n")
      (@ "}\n"))))

(defun output-imported-enums (output)
  (when gtk-imported-enums
    (@ "\f\n/* Imported enums */\n\n")
    (mapc (lambda (enum)
	    (let*
		((cname (gtk-canonical-name (symbol-name (car enum)))))
	      (@ "extern sgtk_enum_info sgtk_%s_info;\n" cname)))
	  gtk-imported-enums)
    (@ "\n")))

(defun output-enums (output)
  (when gtk-enums
    (@ "\f\n/* Enums definitions */\n\n")
    (mapc (lambda (enum)
	    (let*
		((name (car enum))
		 (cname (gtk-canonical-name (symbol-name name)))
		 (values (cdr enum)))
	      ;; write literal names
	      (@ "static sgtk_enum_literal _%s_literals[%d] = {\n"
		 cname (length values))
	      (mapc (lambda (cell)
		      (@ "  { \"%s\", %s },\n" (car cell) (nth 1 cell)))
		    values)
	      (@ "};\n")
	      ;; write type info struct
	      (@ "sgtk_enum_info sgtk_%s_info = {\n" cname)
	      (@ "  { \"%s\", G_TYPE_ENUM }, %d, _%s_literals,\n"
		 name (length values) cname)
	      (@ "};\n\n")))
	  gtk-enums)))

(defun output-imported-string-enums (output)
  (when gtk-imported-string-enums
    (@ "\f\n/* Imported string enums */\n\n")
    (mapc (lambda (enum)
	    (let*
		((cname (gtk-canonical-name (symbol-name (car enum)))))
	      (@ "extern sgtk_string_enum_info sgtk_%s_info;\n" cname)))
	  gtk-imported-string-enums)
    (@ "\n")))

(defun output-string-enums (output)
  (when gtk-string-enums
    (@ "\f\n/* String enums definitions */\n\n")
    (mapc (lambda (enum)
	    (let*
		((name (car enum))
		 (cname (gtk-canonical-name (symbol-name name)))
		 (values (cdr enum)))
	      ;; write literal names
	      (@ "static sgtk_senum_literal _%s_literals[%d] = {\n"
		 cname (length values))
	      (mapc (lambda (cell)
		      (@ "  { \"%s\", %s },\n" (car cell) (nth 1 cell)))
		    values)
	      (@ "};\n")
	      ;; write type info struct
	      (@ "sgtk_senum_info sgtk_%s_info = {\n" cname)
	      (@ "  { \"%s\", G_TYPE_INVALID }, %d, _%s_literals,\n"
		 name (length values) cname)
	      (@ "};\n\n")))
	  gtk-string-enums)))

(defun output-imported-flags (output)
  (when gtk-imported-flags
    (@ "\f\n/* Imported flags */\n\n")
    (mapc (lambda (flag)
	    (let*
		((cname (gtk-canonical-name (symbol-name (car flag)))))
	      (@ "extern sgtk_enum_info sgtk_%s_info;\n" cname)))
	  gtk-imported-flags)
    (@ "\n")))

(defun output-flags (output)
  (when gtk-flags
    (@ "\f\n/* Flags definitions */\n\n")
    (mapc (lambda (flag)
	    (let*
		((name (car flag))
		 (cname (gtk-canonical-name (symbol-name name)))
		 (values (cdr flag)))
	      ;; write literal names
	      (@ "static sgtk_enum_literal _%s_literals[%d] = {\n"
		 cname (length values))
	      (mapc (lambda (cell)
		      (@ "  { \"%s\", %s },\n" (car cell) (nth 1 cell)))
		    values)
	      (@ "};\n")
	      ;; write type info struct
	      (@ "sgtk_enum_info sgtk_%s_info = {\n" cname)
	      (@ "  { \"%s\", G_TYPE_FLAGS }, %d, _%s_literals,\n"
		 name (length values) cname)
	      (@ "};\n\n")))
	  gtk-flags)))

(defun output-imported-boxed (output)
  (when gtk-imported-boxed
    (@ "\f\n/* Imported boxed structures */\n\n")
    (mapc (lambda (boxed)
	    (let*
		((cname (gtk-canonical-name (symbol-name (car boxed)))))
	      (@ "extern sgtk_boxed_info sgtk_%s_info;\n" cname)))
	  gtk-imported-boxed)
    (@ "\n")))

(defun output-boxed (output)
  (when gtk-boxed
    (@ "\f\n/* Boxed structure definitions */\n\n")
    (mapc (lambda (boxed)
	    (let*
		((name (car boxed))
		 (cname (gtk-canonical-name (symbol-name name)))
		 (attrs (cdr boxed))
		 (conv (car (cdr (assq 'conversion attrs)))))
	      (when conv
		(@ "repv %s (repv);\n" conv))
	      (@ "sgtk_boxed_info sgtk_%s_info = {\n" cname)
	      (@ "  { \"%s\", G_TYPE_BOXED, %s },\n" name (or conv "NULL"))
	      (@ "  (void *(*)(void*))%s,\n"
		 (or (car (cdr (assq 'copy attrs))) "NULL"))
	      (@ "  (void (*)(void*))%s,\n"
		 (or (car (cdr (assq 'free attrs))) "NULL"))
	      (@ "  %s\n"
		 (or (car (cdr (assq 'size attrs))) 0))
	      (@ "};\n\n")))
	  gtk-boxed)))

(defun output-imported-objects (output)
  (when gtk-imported-objects
    (@ "\f\n/* Imported GTK objects */\n\n")
    (mapc (lambda (obj)
	    (let*
		((cname (gtk-canonical-name (symbol-name (car obj)))))
	      (@ "extern sgtk_object_info sgtk_%s_info;\n" cname)))
	  gtk-imported-objects)
    (@ "\n")))

(defun output-objects (output)
  (when gtk-objects
    (@ "\f\n/* GTK object definitions */\n\n")
    (mapc (lambda (obj)
	    (let*
		((name (car obj))
		 (cname (gtk-canonical-name (symbol-name name))))
	      (@ "sgtk_object_info sgtk_%s_info = {\n" cname)
	      (@ "  { \"%s\", G_TYPE_OBJECT }, %s_get_type\n" name cname)
	      (@ "};\n\n"))) gtk-objects)))

(defun output-type-info (output)
  (when (or gtk-enums gtk-flags gtk-boxed gtk-objects)
    (@ "\f\n/* Vector of all type information */\n\n")
    (@ "static sgtk_type_info *_type_infos[] = {\n")
    (mapc (lambda (lst)
	    (mapc (lambda (type)
		    (@ "  (sgtk_type_info*)&sgtk_%s_info,\n"
		       (gtk-canonical-name (symbol-name (car type)))))
		  lst))
	  (list gtk-enums gtk-string-enums gtk-flags gtk-boxed gtk-objects))
    (@ "  NULL\n};\n\n")))

(defun output-functions (output)
  (@ "\f\n/* Defuns */\n\n")
  (mapc (lambda (fun)
	  (let
	      ;; send output to a temporary buffer to allow helper
	      ;; functions to be emitted asynchronously
	      ((temporary-stream (make-string-output-stream)))
	    (output-function fun temporary-stream)
	    (write output (get-output-stream-string temporary-stream))))
	gtk-functions)
  (@ "\n\n"))

(defun output-subrs (output)
  (@ "\f\n/* Initialisation */\n\n")
  (let
      ((init-func (gtk-get-option 'init-func gtk-options))
       (other-inits (gtk-get-options 'other-inits gtk-options))
       (extra-init (gtk-get-options 'extra-init-code gtk-options))
       (system-init (gtk-get-options 'system-init-code gtk-options)))
    (when init-func
      (@ "void\n%s (void)\n{\n" init-func)
      (@ "  static int done;\n  if (!done)\n    {\n")
      (@ "      done = 1;\n")
      (mapc (lambda (func)
	      (@ "      %s ();\n" func)) other-inits)
      (when (or gtk-enums gtk-string-enums gtk-flags gtk-boxed gtk-objects)
	(@ "      sgtk_register_type_infos (_type_infos);\n"))
      (mapc (lambda (cname)
	      (@ "      rep_ADD_SUBR(S%s);\n" cname)) (nreverse gtk-subrs))
      (mapc (lambda (code)
	      (declare (unused code))
	      (@ "      %s\n")) extra-init)
      (when system-init
	(@ "      {\n")
	(@ "        char *tem = getenv (\"REP_GTK_DONT_INITIALIZE\");\n")
	(@ "        if (tem == 0 || atoi (tem) == 0) {\n")
	(mapc (lambda (code)
		(@ "          %s\n" code)) system-init)
	(@ "        }\n")
	(@ "      }\n"))
      (@ "    \}\n\}\n"))))

(defun output-gtk (output)
  (output-header output)
  (output-imported-enums output)
  (output-imported-string-enums output)
  (output-imported-flags output)
  (output-imported-boxed output)
  (output-imported-objects output)
  (output-enums output)
  (output-string-enums output)
  (output-flags output)
  (output-boxed output)
  (output-objects output)
  (output-functions output)
  (output-field-functions gtk-boxed output)
  (output-field-functions gtk-objects output)
  (output-type-info output)
  (output-subrs output)
  (output-footer output))


;; Type management

(defun gtk-outer-type (type)
  (while (consp type)
    (setq type (car type)))
  type)

(defun gtk-inner-type (type)
  (while (consp (car type))
    (setq type (car type)))
  (nth 1 type))

(defun gtk-composite-type-mode (type)
  (while (consp (car type))
    (setq type (car type)))
  (case (car type)
    ((ret) 'out)
    ((fvec) (or (nth 3 type) 'in))
    (t (or (nth 2 type) 'in))))

(defun gtk-composite-type-len (type)
  (while (consp (car type))
    (setq type (car type)))
  (case (car type)
    ((ret) 1)
    ((fvec) (nth 2 type))
    (t nil)))

(defun gtk-type-info (type)
  (let*
      ((actual-type (gtk-outer-type type))
       (typage (cond ((or (assq actual-type gtk-enums)
			  (assq actual-type gtk-imported-enums))
		      (assq 'enum gtk-type-alist))
		     ((or (assq actual-type gtk-string-enums)
			  (assq actual-type gtk-imported-string-enums))
		      (assq 'senum gtk-type-alist))
		     ((or (assq actual-type gtk-flags)
			  (assq actual-type gtk-imported-flags))
		      (assq 'flags gtk-type-alist))
		     ((or (assq actual-type gtk-boxed)
			  (assq actual-type gtk-imported-boxed))
		      (assq 'boxed gtk-type-alist))
		     ((or (assq actual-type gtk-objects)
			  (assq actual-type gtk-imported-objects))
		      (assq 'object gtk-type-alist))
		     (t
		      (assq actual-type gtk-type-alist)))))
    (or typage (error "Unknown type: %s" type))))

(defmacro gtk-typage-prop (typage prop)
  `(cdr (assq ,prop (nthcdr 5 ,typage))))

(defun gtk-type-decl (type typage)
  (let
      ((decl (nth 1 typage)))
    (if (functionp decl)
	(funcall decl type typage)
      decl)))

(defmacro gtk-type-fromrep (typage)
  `(nth 2 ,typage))

(defmacro gtk-type-torep (typage)
  `(nth 3 ,typage))

(defmacro gtk-type-pred (typage)
  `(nth 4 ,typage))

(defun gtk-type-prop (type prop)
  (gtk-typage-prop (gtk-type-info type) prop))


;; Function arg helpers

(defmacro gtk-get-arg-options (option arg)
  `(assq ,option (nthcdr 2 ,arg)))

(defun gtk-arg-optional-p (arg)
  (nth 1 (gtk-get-arg-options '= arg)))

(defmacro gtk-arg-type (arg)
  `(car ,arg))

(defmacro gtk-arg-name (arg)
  `(symbol-name (nth 1 ,arg)))


;; Type output functions

(defun output-complex-type (type typage)
  (declare (unused typage))
  (setq type (gtk-outer-type type))
  (if (or (assq type gtk-enums) (assq type gtk-imported-enums)
	  (assq type gtk-flags) (assq type gtk-imported-flags))
      (symbol-name type)
    (format nil "%s*" type)))

(define (output-rep-to-static x)
  (lambda (output type rep-var typage)
    (setq type (gtk-outer-type type))
    (let ((name (gtk-canonical-name (symbol-name type))))
      (@ "\(%s\) sgtk_rep_to_%s \(%s, &sgtk_%s_info\)"
	 (gtk-type-decl type typage) x rep-var name))))

(define (output-static-to-rep x)
  (lambda (output type gtk-var typage)
    (declare (unused typage))
    (setq type (gtk-outer-type type))
    (let ((name (gtk-canonical-name (symbol-name type))))
      (@ "sgtk_%s_to_rep \(%s, &sgtk_%s_info\)" x gtk-var name))))

(define (output-static-pred x)
  (lambda (output type rep-var typage)
    (declare (unused typage))
    (@ "sgtk_valid_%s \(%s, &sgtk_%s_info\)"
       x rep-var (gtk-canonical-name (symbol-name type)))))

(define output-rep-to-enum (output-rep-to-static 'enum))
(define output-enum-to-rep (output-static-to-rep 'enum))
(define output-enum-pred (output-static-pred 'enum))

(define output-rep-to-senum (output-rep-to-static 'senum))
(define output-senum-to-rep (output-static-to-rep 'senum))
(define output-senum-pred (output-static-pred 'senum))

(define output-rep-to-flags (output-rep-to-static 'flags))
(define output-flags-to-rep (output-static-to-rep 'flags))
(define output-flags-pred (output-static-pred 'flags))

(defun output-rep-to-boxed (output type rep-var typage)
  (declare (unused typage))
  (setq type (gtk-outer-type type))
  (@ "\(%s*\) sgtk_rep_to_boxed \(%s\)" type rep-var))

(defun output-boxed-to-rep (output type gtk-var typage)
  (declare (unused typage))
  (let*
      ((base-type (gtk-outer-type type))
       (name (gtk-canonical-name (symbol-name base-type)))
       (copy (if (assq 'copy (cdr type))
		 (gtk-get-option 'copy (cdr type))
	       t)))
    (@ "sgtk_boxed_to_rep \(%s, &sgtk_%s_info, %d\)"
       gtk-var name (if copy 1 0))))

(defun output-boxed-pred (output type rep-var typage)
  (declare (unused typage))
  (@ "sgtk_valid_boxed \(%s, &sgtk_%s_info\)"
     rep-var (gtk-canonical-name (symbol-name type))))

(defun output-rep-to-object (output type rep-var typage)
  (declare (unused typage))
  (setq type (gtk-outer-type type))
  (@ "\(%s*\) sgtk_get_gobj \(%s\)" type rep-var))

(defun output-object-to-rep (output type gtk-var typage)
  (declare (unused typage))
  (setq type (gtk-outer-type type))
  (@ "sgtk_wrap_gobj \(\(GObject*\) %s\)" gtk-var))

(defun output-object-pred (output type rep-var typage)
  (declare (unused typage))
  (@ "sgtk_is_a_gobj \(%s_get_type \(\), %s\)"
     (gtk-canonical-name (symbol-name type)) rep-var))

(defun output-rep-to-full-callback (output type rep-var typage options)
  (declare (unused typage type))
  (let
      ((protect (gtk-get-option 'protection options)))
    (cond ((eq protect '*result*)
	   (@ "sgtk_new_protect \(%s\)" rep-var))
	  ((and (not (eq protect t))
		(not (eq protect nil)))
	   (@ "sgtk_protect \(p_%s, %s\)" protect rep-var))
	  (t
	   (@ "sgtk_protect \(Qt, %s\)" rep-var)))))

(defun output-full-callback-args (output type var options)
  (declare (unused typage type options))
  (@ "0, sgtk_callback_marshal, (gpointer)%s, sgtk_callback_destroy" var))

(defun output-full-callback-finish (output type g-var r-var options)
  (declare (unused typage type r-var))
  (let
      ((protect (gtk-get-option 'protection options)))
    (when (eq protect '*result*)
      (@ "  sgtk_set_protect \(pr_ret, %s\);\n" g-var))))

(defun output-rep-to-gclosure (output type rep-var typage options)
  (declare (unused typage type))
  (let
      ((protect (gtk-get-option 'protection options)))
    (cond ((eq protect '*result*)
	   (@ "sgtk_new_gclosure \(%s\)" rep-var))
	  ((and (not (eq protect t))
		(not (eq protect nil)))
	   (@ "sgtk_gclosure \(p_%s, %s\)" protect rep-var))
	  (t
	   (@ "sgtk_gclosure \(Qt, %s\)" rep-var)))))

(defun output-gclosure-finish (output type g-var r-var options)
  (declare (unused typage type r-var))
  (let
      ((protect (gtk-get-option 'protection options)))
    (when (eq protect '*result*)
      (@ "  sgtk_set_gclosure \(pr_ret, %s\);\n" g-var))))

(defun output-rep-to-cvec (output type rep-var typage)
  (declare (unused typage))
  (let*
      ((inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage))
       (mode (gtk-composite-type-mode type)))
    (output-helper inner-type standard-output)
    (@ "sgtk_rep_to_cvec \(%s, %s, sizeof \(%s\)\)"
       rep-var
       (if (eq mode 'out)
	   "0"
	 (format nil "_sgtk_helper_fromrep_%s" inner-type))
       decl)))

(defun output-cvec-to-rep (output type gtk-var typage)
  (declare (unused typage))
  (let*
      ((inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage)))
    (output-helper inner-type standard-output)
    (@ "sgtk_cvec_to_rep \(&%s, _sgtk_helper_torep_copy_%s, sizeof \(%s\)\)"
       gtk-var inner-type decl)))

(defun output-cvec-pred (output type rep-var typage)
  (declare (unused typage))
  (let*
      ((inner-type (gtk-inner-type type))
       (mode (gtk-composite-type-mode type))
       (len (gtk-composite-type-len type)))
    (output-helper inner-type standard-output)
    (if len
	(@ "sgtk_valid_complen \(%s, %s, %s\)"
	   rep-var
	   (if (eq mode 'out)
	       ;; `out', so don't check inner validity
	       "NULL"
	     (concat "_sgtk_helper_valid_" (symbol-name inner-type)))
	   len)
      (@ "sgtk_valid_composite \(%s, _sgtk_helper_valid_%s\)"
	 rep-var inner-type))))

(defun output-cvec-args (output type var options)
  (declare (unused typage options))
  (let*
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage)))
    (cond ((eq outer-type 'cvec)
	   (@ "%s.count, \(%s*\) %s.vec" var decl var))
	  ((eq outer-type 'cvecr)
	   (@ "\(%s*\) %s.vec, %s.count" decl var var))
	  ((memq outer-type '(fvec ret tvec))
	   (@ "\(%s*\) %s.vec" decl var))
	  (t
	   (gtk-warning "Don't know how to pass type %s" type)))))

(defun output-cvec-finish (output type gtk-var rep-var options)
  (declare (unused typage options))
  (let*
      ((inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage))
       (mode (gtk-composite-type-mode type)))
    (@ "  sgtk_cvec_finish \(&%s, %s, %s, sizeof \(%s\)\);\n"
       gtk-var rep-var
       (if (eq mode 'in)
	   "0"
	 (format nil "_sgtk_helper_torep_nocopy_%s" inner-type))
       decl)))

(defun output-rep-to-list (output type rep-var typage)
  (declare (unused typage))
  (let
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type)))
    (output-helper inner-type standard-output)
    (@ "sgtk_rep_to_%s \(%s, _sgtk_helper_fromrep_%s\)"
       outer-type rep-var inner-type)))

(defun output-list-to-rep (output type gtk-var typage)
  (declare (unused typage))
  (let
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type)))
    (output-helper inner-type standard-output)
    (@ "sgtk_%s_to_rep \(%s, _sgtk_helper_torep_copy_%s\)"
       outer-type gtk-var inner-type)))

(defun output-list-finish (output type gtk-var rep-var options)
  (declare (unused typage options))
  (let
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (mode (gtk-composite-type-mode type)))
    (@ "  sgtk_%s_finish \(%s, %s, %s\);\n"
       outer-type gtk-var rep-var
       (if (eq mode 'in)
	   "0"
	 (format nil "_sgtk_helper_torep_nocopy_%s" inner-type)))))


;; Function generation

(defun output-function (def output #!optional function-callback)
  (let*
      ((ret (nth 1 def))
       (args (nth 2 def))
       (options (nthcdr 3 def))
       (fname (symbol-name (car def)))
       (rname (or (gtk-get-option 'scm-name options)
		  (gtk-hyphenate-name fname)))
       (cname (gtk-unhyphenate-name rname))
       (subrtype (if (or (> (length args) 5)
			 (gtk-get-option 'rest-arg options))
		     'n
		   (length args))))
    (setq gtk-subrs (cons cname gtk-subrs))

    ;; output header
    (@ "DEFUN\(\"%s\", F%s, S%s, \(" rname cname cname)
    (if (eq subrtype 'n)
	(@ "repv args")
      (if (zerop subrtype)
	  (@ "void")
	(let
	    ((tem args))
	  (while tem
	    (@ "repv p_%s%s" (gtk-arg-name (car tem)) (if (cdr tem) ", " ""))
	    (setq tem (cdr tem))))))
    (@ "\), rep_Subr%s\)\n{\n" (if (numberp subrtype) subrtype "N"))
    (unless (eq ret 'none)
      (@ "  repv pr_ret;\n"))
    (when (eq subrtype 'n)
      (@ "  repv ")
      (let
	  ((tem args))
	(while tem
	  (@ "p_%s%s" (gtk-arg-name (car tem)) (if (cdr tem) ", " ";\n\n"))
	  (setq tem (cdr tem)))))

    ;; output any gc roots required
    (mapc (lambda (arg)
	    (when (or (gtk-get-arg-options 'protect-during arg)
		      (gtk-type-prop (gtk-arg-type arg) 'finish))
	      (@ "  rep_GC_root gc_%s;\n" (gtk-arg-name arg)))) args)

    ;; output arg/ret decls
    (mapc (lambda (arg)
	    (let*
		((type (gtk-arg-type arg))
		 (typage (gtk-type-info type))
		 (decl (gtk-type-decl type typage)))
	      (if (stringp decl)
		  (@ "  %s c_%s;\n" decl (gtk-arg-name arg))
		(gtk-warning
		 "Don't know how to declare type: %s" type)))) args)
    (when (gtk-get-option 'gerror-arg options)
      (@ "  GError* error = NULL;\n"))
    (unless (eq ret 'none)
      (let*
	  ((typage (gtk-type-info ret))
	   (decl (gtk-type-decl ret typage)))
	(cond
	 ((stringp decl)
	  (@ "  %s cr_ret;\n" decl))
	 ((functionp decl)
	  (funcall decl output ret "cr_ret" typage options))
	 (t
	  (gtk-warning
	   "Don't know how to declare type: %s" ret)))))
    (unless (and (null args) (eq ret 'none))
      (@ "\n"))

    ;; break out the list of parameters
    (when (eq subrtype 'n)
      (let
	  ((tem args)
	   (i 1))
	(while tem
	  (@ "  if \(!rep_CONSP\(args\)\)\n")
	  (@ "    p_%s = Qnil; \n" (gtk-arg-name (car tem)))
	  (@ "  else {\n")
	  (@ (if (and (null (cdr tem)) (gtk-get-option 'rest-arg options))
		 "    p_%s = args; args = Qnil;\n"
	       "    p_%s = rep_CAR(args); args = rep_CDR(args);\n")
	     (gtk-arg-name (car tem)))
	  (@ "  }\n")
	  (setq tem (cdr tem))
	  (setq i (1+ i)))
	(@ "\n")))

    ;; output arg checks and conversions
    (let
	((tem args)
	 (i 1))
      (while tem
	(let*
	    ((type (gtk-arg-type (car tem)))
	     (typage (gtk-type-info type))
	     (pred (gtk-type-pred typage))
	     (optional (gtk-arg-optional-p (car tem)))
	     (type-options (gtk-get-options type gtk-options)))
	  (when (gtk-get-option 'conversion type-options)
	    (@ "  p_%s = %s \(p_%s\);\n"
	       (gtk-arg-name (car tem))
	       (gtk-get-option 'conversion type-options)
	       (gtk-arg-name (car tem))))
	  (unless (or optional (null pred))
	    (when (gtk-get-arg-options 'null-ok (car tem))
	      (@ "  if (p_%s != Qnil)\n  " (gtk-arg-name (car tem))))
	    (@ "  rep_DECLARE \(%d, p_%s, " i (gtk-arg-name (car tem)))
	    (cond ((stringp pred)
		   (@ "%s \(p_%s\)" pred (gtk-arg-name (car tem))))
		  ((functionp pred)
		   (funcall pred output type
			    (concat "p_" (gtk-arg-name (car tem)))
			    typage options))
		  (t
		   (gtk-warning "Don't know type predicate: %s" type)))
	    (@ "\);\n"))
	  (setq tem (cdr tem))
	  (setq i (1+ i)))))
    (when args
      (@ "\n"))

    ;; initialise gc roots
    (mapc (lambda (arg)
	    (when (or (gtk-get-arg-options 'protect-during arg)
		      (gtk-type-prop (gtk-arg-type arg) 'finish))
	      (@ "  rep_PUSHGC \(gc_%s, p_%s\);\n"
		 (gtk-arg-name arg) (gtk-arg-name arg)))) args)

    ;; output arg initialisations
    (mapc (lambda (arg)
	    (let*
		((type (gtk-arg-type arg))
		 (typage (gtk-type-info type))
		 (from (gtk-type-fromrep typage))
		 (optional (gtk-arg-optional-p arg)))
	      (when (gtk-get-arg-options 'null-ok arg)
		(@ "  if (p_%s == Qnil)\n    c_%s = 0; \n  else\n  "
		   (gtk-arg-name arg) (gtk-arg-name arg)))
	      (when optional
		(@ "  if \(p_%s == Qnil\)\n    c_%s = %s;\n  else\n  "
		   (gtk-arg-name arg) (gtk-arg-name arg) optional))
	      (@ "  c_%s = " (gtk-arg-name arg))
	      (cond ((stringp from)
		     (@ "%s \(p_%s\)" from (gtk-arg-name arg)))
		    ((functionp from)
		     (funcall from output type
			      (concat "p_" (gtk-arg-name arg))
			      typage options))
		    (t
		     (gtk-warning
		      "Don't know how to convert repv to %s" type)))
	      (@ ";\n"))) args)
    (when args
      (@ "\n"))

    (if function-callback
	(funcall function-callback output)
      ;; output call
      (@ "  ")
      (unless (eq ret 'none)
	(@ "cr_ret = "))
      (@ "%s \(" fname)
      (let
	  ((tem args))
	(while tem
	  (let
	      ((opt (gtk-type-prop (gtk-arg-type (car tem)) 'c2args)))
	    (if opt
		(if (functionp opt)
		    (funcall opt output (gtk-arg-type (car tem))
			     (concat "c_" (gtk-arg-name (car tem)))
			     options)
		  (gtk-warning "c2args function %s undefined" opt))
	      (@ "c_%s" (gtk-arg-name (car tem)))))
	  (@ (if (cdr tem) ", " ""))
	  (setq tem (cdr tem))))
      (if (gtk-get-option 'gerror-arg options)
	  (@ ", &error"))
      (@ "\);\n\n"))

    ;; output ret conversion
    (unless (eq ret 'none)
      (let*
	  ((typage (gtk-type-info ret))
	   (to (gtk-type-torep typage)))
	(@ "  pr_ret = ")
	(cond ((stringp to)
	       (@ "%s \(cr_ret\)" to))
	      ((functionp to)
	       (funcall to output ret "cr_ret" typage options))
	      (t
	       (gtk-warning
		"Don't know how to convert %s to repv" ret)))
	(@ ";\n")))

    ;; output `finish' options
    (mapc (lambda (arg)
	    (let
		((opt (gtk-type-prop (gtk-arg-type arg) 'finish)))
	      (when opt
		(if (functionp opt)
		    (funcall opt output (gtk-arg-type arg)
			     (concat "c_" (gtk-arg-name arg))
			     (concat "p_" (gtk-arg-name arg))
			     options)
		  (gtk-warning "finish function %s undefined" opt))))) args)

    ;; pop gc roots
    (mapc (lambda (arg)
	    (when (or (gtk-get-arg-options 'protect-during arg)
		      (gtk-type-prop (gtk-arg-type arg) 'finish))
	      (@ "  rep_POPGC;\n"
		 (gtk-arg-name arg) (gtk-arg-name arg)))) args)

    ;; gerror checking
    (when (gtk-get-option 'gerror-arg options)
      (@ "  if (error != NULL)\n" )
      (@ "    sgtk_throw_gerror (\"%s\", error);\n" fname))
    
    ;; output return statement
    (if (eq ret 'none)
	(@ "  return Qnil;\n")
      (@ "  return pr_ret;\n"))

    ;; footer
    (@ "}\n\n")))


;; Field access functions

(defun output-field-functions (type-list output)
  (mapc (lambda (def)
	  (let
	      ((fields (cdr (assq 'fields (cdr def)))))
	    (when fields
	      (mapc #'(lambda (field)
			(output-field-accessors
			 (car def) field output
			 (car (cdr (assq 'setter (nthcdr 2 field))))
			 (car (cdr (assq 'getter (nthcdr 2 field))))))
		    fields))
	    (output-type-predicate (car def) output)))
	type-list))

(defun output-field-accessors (datatype field output #!optional settable getter)
  (let*
      ((type (car field))
       (cdatatype (gtk-canonical-name (symbol-name datatype)))
       (cfieldname (symbol-name (nth 1 field))))
    (output-function (list (intern (format nil "%s_%s" cdatatype cfieldname))
			   type (list (list datatype 'obj)))
		     output
		     (lambda (output)
		       (if getter
			   (@ "  cr_ret = %s (c_obj);\n" getter)
			 (@ "  cr_ret = c_obj->%s;\n" cfieldname))))
    (when settable
      (output-function (list (intern (format nil "%s_%s_set"
					     cdatatype cfieldname))
			     'none (list (list datatype 'obj)
					 (list type 'data)))
		       output
		       (lambda (output)
			 (@ "  c_obj->%s = c_data;\n" cfieldname))))))

(defun output-type-predicate (type output)
  (let*
      ((typage (gtk-type-info type))
       (ctype (gtk-canonical-name (symbol-name type)))
       (rtype (gtk-hyphenate-name ctype))
       (pred (gtk-type-pred typage)))
    (cond ((stringp pred)
	   (setq pred (format nil "%s \(p_obj\)" pred)))
	  ((functionp pred)
	   (let
	       ((temporary-output (make-string-output-stream)))
	     (funcall pred temporary-output type "p_obj" typage nil)
	     (setq pred (get-output-stream-string temporary-output))))
	  ((null pred)
	   (setq pred "1")))
    (@ "DEFUN\(\"%s-p\", F%s_p, S%s_p, \(repv p_obj\), rep_Subr1\)\n{\n"
       rtype ctype ctype)
    (@ "  return \(%s\) ? Qt : Qnil;\n}\n\n" pred)
    (setq gtk-subrs (cons (intern (format nil "%s_p" ctype)) gtk-subrs))))


;; Composite type helper functions

(defun output-helper (type output)
  (unless (memq type gtk-emitted-composite-helpers)
    (setq gtk-emitted-composite-helpers
	  (cons type gtk-emitted-composite-helpers))
    (let*
	((typage (gtk-type-info type))
	 (pred (gtk-type-pred typage))
	 (decl (gtk-type-decl type typage))
	 (from (gtk-type-fromrep typage))
	 (to (gtk-type-torep typage)))

      ;; use some hackery to get from, to, and pred functions as strings
      (cond ((stringp from)
	     (setq from (concat from " \(obj\)")))
	    ((functionp from)
	     (let
		 ((temporary-output (make-string-output-stream)))
	       (funcall from temporary-output type "obj" typage nil)
	       (setq from (get-output-stream-string temporary-output)))))
      (cond ((stringp to)
	     (setq to (format nil "%s \(*\(%s*\)mem\)" to decl)))
	    ((functionp to)
	     (let
		 ((temporary-output (make-string-output-stream)))
	       (funcall to temporary-output type
			(format nil "\(*\(%s*\)mem\)" decl) typage nil)
	       (setq to (get-output-stream-string temporary-output)))))
      (cond ((stringp pred)
	     (setq pred (format nil "%s \(obj\)" pred)))
	    ((functionp pred)
	     (let
		 ((temporary-output (make-string-output-stream)))
	       (funcall pred temporary-output type "obj" typage nil)
	       (setq pred (get-output-stream-string temporary-output))))
	    ((null pred)
	     (setq pred "1")))

      (unless (and (stringp decl) (stringp pred) (stringp from) (stringp to))
	(error "Can't create composite helper for %s" type))
      (@ "/* helpers for %s */\n" type)
      (@ "static int\n_sgtk_helper_valid_%s \(repv obj\)\n" type)
      (@ "\{\n  return obj == Qnil || \(%s\);\n\}\n" pred)
      (@ "static void\n_sgtk_helper_fromrep_%s \(repv obj, void *mem\)\n" type)
      (@ "\{\n  *\(%s*\)mem = %s;\n\}\n" decl from)
      (@ "static repv\n_sgtk_helper_torep_copy_%s \(void *mem\)\n" type)
      (@ "\{\n  return %s;\n\}\n" to)
      ;; XXX presumably there should be a difference between the
      ;; XXX copy and no_copy variants!?
      (@ "static repv\n_sgtk_helper_torep_nocopy_%s \(void *mem\)\n" type)
      (@ "\{\n  return %s;\n\}\n\n" to))))


;; Sundries

(defun gtk-canonical-name (name)
  (let
      ((out nil)
       (point 0))

    ;; Some Classes (GtkUIManager) contain Upcase Tokens: UI
    (while (string-match "[A-Z]([A-Z]+)[A-Z]" name)
      (let ((upcase-token (substring name (match-start 1) (match-end 1))))
	(setq name (string-replace upcase-token (string-downcase upcase-token) name))))

    (while (string-match "[A-Z]+" name point)
      (setq out (cons (substring name point (match-start)) out))
      (unless (zerop point)
	(setq out (cons ?_ out)))
      (setq out (cons (translate-string (substring
					 name (match-start) (match-end))
					downcase-table) out))
      (setq point (match-end)))
    (if out
	(progn
	  (setq out (cons (substring name point) out))
	  (apply concat (nreverse out)))
      name)))

(defun gtk-hyphenate-name (name)
  (if (string-match "_" name)
      (translate-string (copy-sequence name) gtk-hyphen-map)
    name))

(defun gtk-unhyphenate-name (name)
  (if (string-match "-" name)
      (translate-string (copy-sequence name) gtk-unhyphen-map)
    name))

(defun gtk-warning (fmt . args)
  (apply format standard-error fmt args)
  (write standard-error ?\n))


;; initialisation

(define-type 'type "GtkType" "sgtk_rep_to_type"
	     "sgtk_type_to_rep" "sgtk_valid_type")

(define-type 'GValue "GValue" "sgtk_rep_to_gvalue"
             "sgtk_gvalue_to_rep" "sgtk_valid_gvalue")

(define-type 'GtkArg "GtkArg" "sgtk_rep_to_arg"
             "sgtk_arg_to_rep" "sgtk_valid_arg")

(define-type 'char "gchar" "sgtk_rep_to_char"
	     "sgtk_char_to_rep" "sgtk_valid_char")

(define-type 'bool "int" "sgtk_rep_to_bool" "sgtk_bool_to_rep" nil)

;; XXX fix the validation functions
(define-type 'short "short" "sgtk_rep_to_int" "sgtk_int_to_rep"
	     "sgtk_valid_int" '(listable . t))

(define-type 'ushort "gushort" "sgtk_rep_to_uint" "sgtk_uint_to_rep"
	     "sgtk_valid_uint" '(listable . t))

(define-type 'int "gint" "sgtk_rep_to_int" "sgtk_int_to_rep"
	     "sgtk_valid_int" '(listable . t))

(define-type 'uint "guint" "sgtk_rep_to_uint" "sgtk_uint_to_rep"
	     "sgtk_valid_uint" '(listable . t))

(define-type 'GQuark "guint" "sgtk_rep_to_uint" "sgtk_uint_to_rep"
	     "sgtk_valid_uint" '(listable . t))

(define-type 'long "glong" "sgtk_rep_to_long"
	     "sgtk_long_to_rep" "sgtk_valid_long")

(define-type 'ulong "gulong" "sgtk_rep_to_ulong"
	     "sgtk_ulong_to_rep" "sgtk_valid_ulong")

(define-type 'float "gfloat" "sgtk_rep_to_float"
	     "sgtk_float_to_rep" "sgtk_valid_float")

(define-type 'string "char*" "sgtk_rep_to_string"
	     "sgtk_string_to_rep" "sgtk_valid_string" '(listable . t))

(define-type 'enum output-complex-type output-rep-to-enum
	     output-enum-to-rep output-enum-pred)

(define-type 'senum "char*" output-rep-to-senum
	     output-senum-to-rep output-senum-pred)

(define-type 'flags output-complex-type output-rep-to-flags
	      output-flags-to-rep output-flags-pred)

(define-type 'boxed output-complex-type output-rep-to-boxed
	     output-boxed-to-rep output-boxed-pred '(listable . t))

(define-type 'GPointer "gpointer" "sgtk_rep_to_pointer"
	     "sgtk_pointer_to_rep" "sgtk_valid_pointer")

(define-type 'object output-complex-type output-rep-to-object
	     output-object-to-rep output-object-pred '(listable . t))

(define-type 'static_string "const char*" nil
	     "sgtk_static_string_to_rep" nil '(listable . t))

(define-type 'full-callback "sgtk_protshell*" output-rep-to-full-callback nil
	     "sgtk_valid_function" (cons 'c2args output-full-callback-args)
	     (cons 'finish output-full-callback-finish))

(define-type 'GClosure "GClosure*" output-rep-to-gclosure nil
	     "sgtk_valid_function" (cons 'finish output-full-callback-finish))

(define-type 'file-descriptor "int" "sgtk_rep_to_fd"
	     "sgtk_fd_to_rep" "sgtk_valid_fd")

(define-type 'list "GList*" output-rep-to-list output-list-to-rep
	     output-cvec-pred (cons 'finish output-list-finish))

(define-type 'slist "GSList*" output-rep-to-list output-list-to-rep
	     output-cvec-pred (cons 'finish output-list-finish))

(define-type 'cvec "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	     output-cvec-pred (cons 'finish output-cvec-finish)
	     (cons 'c2args output-cvec-args))

(define-type 'cvecr "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	     output-cvec-pred (cons 'finish output-cvec-finish)
	     (cons 'c2args output-cvec-args))

(define-type 'fvec "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	     output-cvec-pred (cons 'finish output-cvec-finish)
	     (cons 'c2args output-cvec-args))

(define-type 'tvec "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	     output-cvec-pred (cons 'finish output-cvec-finish)
	     (cons 'c2args output-cvec-args))

(define-type 'ret "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	     output-cvec-pred (cons 'finish output-cvec-finish)
	     (cons 'c2args output-cvec-args))

(define-type 'double "gdouble" "sgtk_rep_to_double"
	     "sgtk_double_to_rep" "sgtk_valid_double")

(define-type 'GdkPoint "GdkPoint" "sgtk_rep_to_point"
	     "sgtk_point_to_rep" "sgtk_valid_point")

(define-type 'rect "GdkRectangle" "sgtk_rep_to_rect"
	     "sgtk_rect_to_rep" "sgtk_valid_rect")

(define-type 'SCM "repv" "" "" nil)
