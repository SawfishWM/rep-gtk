#! /bin/sh
exec rep --batch "$0" "$@"
!#

(structure ()

    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  (define titles ["name" "uid" "gid" "passwd" "gecos" "home" "shell"])

  (define window (gtk-window-new 'toplevel))

  (define scrolled-window (gtk-scrolled-window-new))

  (define clist (gtk-clist-new-with-titles titles))

  (gtk-container-add window scrolled-window)
  (gtk-container-add scrolled-window clist)
  (gtk-scrolled-window-set-policy scrolled-window 'automatic 'automatic)
  (gtk-window-set-default-size window 400 200)

  (do ((i 0 (1+ i)))
      ((= i 10))
    (gtk-clist-append clist (vector (format nil "record-%d" i)
				    (format nil "field1-%d" i)
				    (format nil "field2-%d" i)
				    (format nil "field3-%d" i)
				    (format nil "field4-%d" i)
				    (format nil "field5-%d" i)
				    (format nil "field6-%d" i))))

  (do ((i 0 (1+ i)))
      ((= i (length titles)))
    (gtk-clist-set-column-auto-resize clist i t))

  (g-signal-connect window "delete_event" (lambda (w) (throw 'quit 0)))

  (g-signal-connect clist "select_row"
		    (lambda args
		      (format standard-error "select: %S\n" args)))

  (gtk-widget-show-all window)

  (setq interrupt-mode 'exit)
  (recursive-edit))

;; Local variables:
;; major-mode: lisp-mode
;; End:
