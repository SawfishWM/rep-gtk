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

;; Notes:
;;
;; This assumes that the `sed-fix-defs' sed script has been run over all
;; input files (to convert schemey things to their lispy equivalents)
;;
;; Todo:
;;  * make the `import' directive work as in guile-gtk?
;;  * doesn't check for `listable' type-property
;;
;; WARNING: This makes some pretty gruesome assumptions. [where?]


;; Configuration

;; Alist of (TYPE ["C-TYPE" | DECL-FUNC] ["REP2GTK" | FROM-REP-FUNC]
;;           ["GTK2REP" | TO-REP-FUNC] ["PRED-NAME" | PRED-FUNC]
;;	     . OPTION-ALIST)
;;
;; The required functions are called as:
;;
;;   (DECL-FUNC TYPE TYPE-INFO)
;;   (FROM-REP-FUNC OUTPUT-STREAM TYPE "REP-VAR" TYPE-INFO OPTIONS)
;;   (TO-REP-FUNC OUTPUT-STREAM TYPE "GTK-VAR" TYPE-INFO OPTIONS)
;;   (PRED-FUNC OUTPUT-STREAM TYPE "REP-VAR" TYPE-INFO OPTIONS)
;;
;; The options in the OPTION-ALIST may be:
;;
;;   (c2args . EMIT-ARG-FUNC)
;;   (finish . FINISH-ARG-FUNC)
;;   (listable . BOOLEAN)
;;
;; with:
;;
;;   (EMIT-ARG-FUNC OUTPUT TYPE "GTK-VAR" OPTIONS)
;;   (FINISH-ARG-FUNC OUTPUT TYPE "GTK-VAR" "REP-VAR" OPTIONS)
;;
(defvar gtk-type-alist nil)


;; Work variables

(defvar gtk-enums nil
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
       (gtk-flags nil)
       (gtk-boxed nil)
       (gtk-objects nil)
       (gtk-functions nil)
       (gtk-options nil)
       (gtk-subrs nil)
       (gtk-emitted-composite-helpers nil))
    (let
	((defs-file (open-file defs-file-name 'read)))
      (or defs-file (error "Can't open input file: %s" defs-file-name))
      (unwind-protect
	  (parse-gtk defs-file)
	(close-file defs-file)))
    (setq gtk-enums (nreverse gtk-enums))
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
	  ;(format standard-error "read: %S\n" def)
	  (when def
	    (or (consp def) (error "Definition isn't a list"))
	    (cond
	     ((eq (car def) 'import)
	      (let
		  ((file (open-file (expand-file-name (nth 1 def)
						      (file-name-directory
						       (file-binding input)))
				    'read)))
		(or file (error "Can't open input file: %s" (nth 1 def)))
		(unwind-protect
		    (parse-gtk file)
		  (close-file file))))
	     ((eq (car def) 'define-enum)
	      (let*
		  ((name (nth 1 def))
		   (body (nthcdr 2 def))
		   (cell (assq name gtk-enums)))
		(if cell
		    (rplacd cell body)
		  (setq gtk-enums (cons (cons name body) gtk-enums)))))
	     ((eq (car def) 'define-flags)
	      (let*
		  ((name (nth 1 def))
		   (body (nthcdr 2 def))
		   (cell (assq name gtk-flags)))
		(if cell
		    (rplacd cell body)
		  (setq gtk-flags (cons (cons name body) gtk-flags)))))
	     ((eq (car def) 'define-boxed)
	      (let
		  ((cell (assq (nth 1 def) gtk-boxed)))
		(if cell
		    (rplacd cell (nthcdr 2 def))
		  (setq gtk-boxed (cons (cdr def) gtk-boxed)))))
	     ((eq (car def) 'define-object)
	      (let*
		  ((name (nth 1 def))
		   (super (nth 2 def))
		   (attrs (nthcdr 3 def))
		   (cell (assq name gtk-objects)))
		(when (car super)
		  (setq attrs (cons (cons 'super (car super)) attrs)))
		(if cell
		    (rplacd cell attrs)
		  (setq gtk-objects (cons (cons name attrs) gtk-objects)))))
	     ((eq (car def) 'define-func)
	      (let
		  ((cell (assq (nth 1 def) gtk-functions)))
		(if cell
		    (rplacd cell (nthcdr 2 def))
		  (setq gtk-functions (cons (cdr def) gtk-functions)))))
	     ((eq (car def) 'options)
	      (mapc (lambda (cell)
		      (let
			  ((value (assq (car cell) gtk-options)))
			(if value
			    (rplacd value (nconc (cdr value)
						 (list (nth 1 cell))))
			  (setq gtk-options (cons cell gtk-options)))))
		    (cdr def)))
	     ((eq (car def) 'add-options)
	      (let
		  ((value (assq (nth 1 def) gtk-options)))
		(if value
		    (rplacd value (nconc (cdr value) (nthcdr 2 def)))
		  (setq gtk-options (cons (cdr def) gtk-options)))))
	     (t
	      (gtk-warning "Ignoring `%S'" def))))))
    (end-of-stream)))


;; Code generation

(defmacro @ (&rest args)
  (list* 'format 'output args))

(defun output-header (output)
  (@ "/* Automatically generated by build-gtk, DO NOT EDIT! */\n\n")
  (when (gtk-get-options 'includes gtk-options)
    (mapc (lambda (opt)
	    (@ "%s\n" opt))
	  (gtk-get-options 'includes gtk-options)))
  (@ "#include <rep.h>\n")
  (@ "#include \"rep-gtk.h\"\n\n"))

(defun output-footer (output))

(defun output-enums (output)
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
		    (@ "  { \"%s\", %s },\n" (car cell) (nth 1 cell))) values)
	    (@ "};\n")
	    ;; write type info struct
	    (@ "sgtk_enum_info sgtk_%s_info = {\n" cname)
	    (@ "  { \"%s\", GTK_TYPE_ENUM }, %d, _%s_literals,\n"
	       name (length values) cname)
	    (@ "};\n\n")))
	gtk-enums))

(defun output-flags (output)
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
		    (@ "  { \"%s\", %s },\n" (car cell) (nth 1 cell))) values)
	    (@ "};\n")
	    ;; write type info struct
	    (@ "sgtk_enum_info sgtk_%s_info = {\n" cname)
	    (@ "  { \"%s\", GTK_TYPE_FLAGS }, %d, _%s_literals,\n"
	       name (length values) cname)
	    (@ "};\n\n")))
	gtk-flags))

(defun output-boxed (output)
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
	    (@ "  { \"%s\", GTK_TYPE_BOXED, %s },\n" name (or conv "NULL"))
	    (@ "  (void *(*)(void*))%s,\n"
	       (or (car (cdr (assq 'copy attrs))) "NULL"))
	    (@ "  (void (*)(void*))%s,\n"
	       (or (car (cdr (assq 'free attrs))) "NULL"))
	    (@ "  %s\n"
	       (or (car (cdr (assq 'size attrs))) 0))
	    (@ "};\n\n")))
	gtk-boxed))

(defun output-objects (output)
  (@ "\f\n/* GTK object definitions */\n\n")
  (mapc (lambda (obj)
	  (let*
	      ((name (car obj))
	       (cname (gtk-canonical-name (symbol-name name))))
	    (@ "sgtk_object_info sgtk_%s_info = {\n" cname)
	    (@ "  { \"%s\", GTK_TYPE_OBJECT }, %s_get_type\n" name cname)
	    (@ "};\n\n"))) gtk-objects))

(defun output-type-info (output)
  (@ "\f\n/* Vector of all type information */\n\n")
  (@ "sgtk_type_info *sgtk_type_infos[] = {\n")
  (mapc (lambda (lst)
	  (mapc (lambda (type)
		  (@ "  (sgtk_type_info*)&sgtk_%s_info,\n"
		     (gtk-canonical-name (symbol-name (car type)))))
		lst))
	(list gtk-enums gtk-flags gtk-boxed gtk-objects))
  (@ "  NULL\n};\n\n"))

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
  (@ "\f\n/* Vector of all subrs */\n\n")
  (@ "rep_xsubr *sgtk_subrs[] = \{\n")
  (mapc (lambda (cname)
	  (@ "  &S%s,\n" cname)) (nreverse gtk-subrs))
  (@ "  0\n\};\n"))

(defun output-gtk (output)
  (output-header output)
  (output-enums output)
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

(defun gtk-inner-type-options (type)
  (while (consp (car type))
    (setq type (car type)))
  (nthcdr 2 type))

(defun gtk-type-info (type)
  (let*
      ((actual-type (gtk-outer-type type))
       (typage (cond ((assq actual-type gtk-enums)
		      (assq 'enum gtk-type-alist))
		     ((assq actual-type gtk-flags)
		      (assq 'flags gtk-type-alist))
		     ((assq actual-type gtk-boxed)
		      (assq 'boxed gtk-type-alist))
		     ((assq actual-type gtk-objects)
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
  `(nth 4 typage))

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
  (setq type (gtk-outer-type type))
  (if (or (assq type gtk-enums) (assq type gtk-flags))
      (symbol-name type)
    (format nil "%s*" type)))

(defun output-rep-to-enum (output type rep-var typage)
  (setq type (gtk-outer-type type))
  (let
      ((name (gtk-canonical-name (symbol-name type))))
    (@ "\(%s\) sgtk_rep_to_enum \(%s, &sgtk_%s_info\)" type rep-var name)))

(defun output-enum-to-rep (output type gtk-var typage)
  (setq type (gtk-outer-type type))
  (let
      ((name (gtk-canonical-name (symbol-name type))))
    (@ "sgtk_enum_to_rep \(%s, &sgtk_%s_info\)" gtk-var name)))

(defun output-enum-pred (output type rep-var typage)
  (@ "sgtk_valid_enum \(%s, &sgtk_%s_info\)"
     rep-var (gtk-canonical-name (symbol-name type))))

(defun output-rep-to-flags (output type rep-var typage)
  (setq type (gtk-outer-type type))
  (let
      ((name (gtk-canonical-name (symbol-name type))))
    (@ "\(%s\) sgtk_rep_to_flags \(%s, &sgtk_%s_info\)" type rep-var name)))

(defun output-flags-to-rep (output type gtk-var typage)
  (setq type (gtk-outer-type type))
  (let
      ((name (gtk-canonical-name (symbol-name type))))
    (@ "sgtk_flags_to_rep \(%s, &sgtk_%s_info\)" gtk-var name)))

(defun output-flags-pred (output type rep-var typage)
  (@ "sgtk_valid_flags \(%s, &sgtk_%s_info\)"
     rep-var (gtk-canonical-name (symbol-name type))))

(defun output-rep-to-boxed (output type rep-var typage)
  (setq type (gtk-outer-type type))
  (@ "\(%s*\) sgtk_rep_to_boxed \(%s\)" type rep-var))

(defun output-boxed-to-rep (output type gtk-var typage)
  (let*
      ((base-type (gtk-outer-type type))
       (name (gtk-canonical-name (symbol-name base-type)))
       (copy (if (assq 'copy (cdr type))
		 (gtk-get-option 'copy (cdr type))
	       t)))
    (@ "sgtk_boxed_to_rep \(%s, &sgtk_%s_info, %d\)"
       gtk-var name (if copy 1 0))))

(defun output-boxed-pred (output type rep-var typage)
  (@ "sgtk_valid_boxed \(%s, &sgtk_%s_info\)"
     rep-var (gtk-canonical-name (symbol-name type))))

(defun output-rep-to-object (output type rep-var typage)
  (setq type (gtk-outer-type type))
  (@ "\(%s*\) sgtk_get_gtkobj \(%s\)" type rep-var))

(defun output-object-to-rep (output type gtk-var typage)
  (setq type (gtk-outer-type type))
  (@ "sgtk_wrap_gtkobj \(\(GtkObject*\) %s\)" gtk-var))

(defun output-object-pred (output type rep-var typage)
  (@ "sgtk_is_a_gtkobj \(%s_get_type \(\), %s\)"
     (gtk-canonical-name (symbol-name type)) rep-var))

(defun output-rep-to-full-callback (output type rep-var typage options)
  (let
      ((protect (gtk-get-option 'protection options)))
    (if (and (not (eq protect t))
	     (not (eq protect nil)))
	(@ "sgtk_protect \(p_%s, %s\)" protect rep-var)
      (@ "sgtk_protect \(Qt, %s\)" rep-var))))

(defun output-full-callback-args (output type var options)
  (@ "0, sgtk_callback_marshal, (gpointer)%s, sgtk_callback_destroy" var))

(defun output-rep-to-cvec (output type rep-var typage)
  (let*
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage)))
    (output-helper inner-type standard-output)
    (@ "sgtk_rep_to_cvec \(%s, %s, sizeof \(%s\)\)"
       rep-var
       (if (eq outer-type 'ret)
	   "0"
	 ;; all but `ret' args are `in'
	 (format nil "_sgtk_helper_fromrep_%s" inner-type))
       decl)))

(defun output-cvec-to-rep ()
  (error "output-rep-to-cvec is unimplemented"))

(defun output-cvec-pred (output type rep-var typage)
  (let*
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type)))
    (output-helper inner-type standard-output)
    (cond ((memq outer-type '(cvec cvecr list slist))
	   ;; XXX assumes `in' or `inout' types
	   (@ "sgtk_valid_composite \(%s, _sgtk_helper_valid_%s\)"
	      rep-var inner-type))
	  ((memq outer-type '(fvec ret))
	   (let
	       ((len (if (eq outer-type 'ret)
			 1
		       (car (gtk-inner-type-options type)))))
	     (@ "sgtk_valid_complen \(%s, %s, %s\)"
		rep-var (if (eq outer-type 'ret)
			    ;; ret is `out', so don't check inner validity
			    "NULL"
			  (concat "_sgtk_helper_valid_"
				  (symbol-name inner-type))) len)))
	  (t
	   (gtk-warning "Don't know type predicate of %s" type)))))

(defun output-cvec-args (output type var options)
  (let*
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage)))
    (cond ((eq outer-type 'cvec)
	   (@ "%s.count, \(%s*\) %s.vec" var decl var))
	  ((eq outer-type 'cvecr)
	   (@ "\(%s*\) %s.vec, %s.count" decl var var))
	  ((eq outer-type 'fvec)
	   (@ "\(%s*\) %s.vec" decl var))
	  ((eq outer-type 'ret)
	   (@ "\(%s*\) %s.vec" decl var))
	  (t
	   (gtk-warning "Don't know how to pass type %s" type)))))

(defun output-cvec-finish (output type gtk-var rep-var options)
  (let*
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type))
       (inner-typage (gtk-type-info inner-type))
       (decl (gtk-type-decl inner-type inner-typage)))
    (@ "  sgtk_cvec_finish \(&%s, %s, %s, sizeof \(%s\)\);\n"
       gtk-var rep-var
       (if (eq outer-type 'ret)
	   ;; only `ret' args are `out'
	   (format nil "_sgtk_helper_torep_nocopy_%s" inner-type)
	 "0")
       decl)))

(defun output-rep-to-list (output type rep-var typage)
  (let
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type)))
    (output-helper inner-type standard-output)
    (@ "sgtk_rep_to_%s \(%s, _sgtk_helper_fromrep_%s\)"
       outer-type rep-var inner-type)))
       
(defun output-list-to-rep (output type gtk-var typage)
  (let
      ((outer-type (gtk-outer-type type))
       (inner-type (gtk-inner-type type)))
    (output-helper inner-type standard-output)
    (@ "sgtk_%s_to_rep \(%s, _sgtk_helper_torep_copy_%s\)"
       outer-type gtk-var inner-type)))

(defun output-list-finish (output type gtk-var rep-var options)
  (let
      ((outer-type (gtk-outer-type type)))
    (@ "  sgtk_%s_finish \(%s, %s, 0\);\n" outer-type gtk-var rep-var)))


;; Function generation

(defun output-function (def output &optional function-callback)
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
	    (when (gtk-type-prop (gtk-arg-type arg) 'finish)
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
	    (when (gtk-type-prop (gtk-arg-type arg) 'finish)
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
      (@ "\);\n\n"))

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

    ;; output ret conversion & return
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

    ;; pop gc roots
    (mapc (lambda (arg)
	    (when (gtk-type-prop (gtk-arg-type arg) 'finish)
	      (@ "  rep_POPGC;\n"
		 (gtk-arg-name arg) (gtk-arg-name arg)))) args)

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
			 (car (cdr (assq 'setter (nthcdr 2 field))))))
		    fields))
	    (output-type-predicate (car def) output)))
	type-list))

(defun output-field-accessors (datatype field output &optional settable)
  (let*
      ((type (car field))
       (cdatatype (gtk-canonical-name (symbol-name datatype)))
       (cfieldname (symbol-name (nth 1 field))))
    (output-function (list (intern (format nil "%s_%s" cdatatype cfieldname))
			   type (list (list datatype 'obj)))
		     output
		     (lambda (output)
		       (@ "  cr_ret = c_obj->%s;\n" cfieldname)))
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

(defun gtk-warning (fmt &rest args)
  (apply format standard-error fmt args)
  (write standard-error ?\n))


;; initialisation

(setq gtk-type-alist
  (list (list 'type "GtkType" "sgtk_rep_to_type" "sgtk_type_to_rep"
	      "sgtk_valid_type")
	(list 'char "gchar" "sgtk_rep_to_char" "sgtk_char_to_rep"
	      "sgtk_valid_char")
	(list 'bool "int" "sgtk_rep_to_bool" "sgtk_bool_to_rep" nil)
	(list 'int "gint" "sgtk_rep_to_int" "sgtk_int_to_rep"
	      "sgtk_valid_int" '(listable . t))
	(list 'uint "guint" "sgtk_rep_to_uint" "sgtk_uint_to_rep"
	      "sgtk_valid_uint" '(listable . t))
	(list 'long "glong" "sgtk_rep_to_long" "sgtk_long_to_rep"
	      "sgtk_valid_long")
	(list 'ulong "gulong" "sgtk_rep_to_ulong" "sgtk_ulong_to_rep"
	      "sgtk_valid_ulong")
	(list 'float "gfloat" "sgtk_rep_to_float" "sgtk_float_to_rep"
	      "sgtk_valid_float")
	(list 'string "char*" "sgtk_rep_to_string" "sgtk_string_to_rep"
	      "sgtk_valid_string" '(listable . t))
	(list 'enum output-complex-type output-rep-to-enum output-enum-to-rep
	      output-enum-pred)
	(list 'flags output-complex-type output-rep-to-flags
	      output-flags-to-rep output-flags-pred)
	(list 'boxed output-complex-type output-rep-to-boxed
	      output-boxed-to-rep output-boxed-pred '(listable . t))
	(list 'pointer "gpointer" "sgtk_rep_to_pointer" "sgtk_pointer_to_rep"
	      "sgtk_pointerp")
	(list 'object output-complex-type output-rep-to-object
	      output-object-to-rep output-object-pred '(listable . t))
	(list 'static_string "char*" nil "sgtk_static_string_to_rep" nil
	      '(listable . t))
	(list 'full-callback "repv" output-rep-to-full-callback nil
	      "sgtk_valid_function" (cons 'c2args output-full-callback-args))
	(list 'file-descriptor "int" "sgtk_rep_to_fd" nil "sgtk_valid_fd")
	(list 'list "GList*" output-rep-to-list output-list-to-rep
	      output-cvec-pred (cons 'finish output-list-finish))
	(list 'slist "GSList*" output-rep-to-list output-list-to-rep
	      output-cvec-pred (cons 'finish output-list-finish))
	(list 'cvec "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	      output-cvec-pred (cons 'finish output-cvec-finish)
	      (cons 'c2args output-cvec-args))
	(list 'cvecr "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	      output-cvec-pred (cons 'finish output-cvec-finish)
	      (cons 'c2args output-cvec-args))
	(list 'fvec "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	      output-cvec-pred (cons 'finish output-cvec-finish)
	      (cons 'c2args output-cvec-args))
	(list 'ret "sgtk_cvec" output-rep-to-cvec output-cvec-to-rep
	      output-cvec-pred (cons 'finish output-cvec-finish)
	      (cons 'c2args output-cvec-args))
	;;(list 'double "gdouble" "sgtk_rep_to_double" "sgtk_double_to_rep"
	;;      "sgtk_doublep")
	(list 'point "GdkPoint" "sgtk_rep_to_point" "sgtk_point_to_rep"
	      "sgtk_valid_point")
	(list 'SCM "repv" "" "" nil)))
