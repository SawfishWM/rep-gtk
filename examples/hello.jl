#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(structure ()

    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  (define window (gtk-window-new 'toplevel))

  (define button (gtk-button-new-with-label "say hello"))

  (gtk-container-set-border-width window 10)

  (g-signal-connect window "delete_event" (lambda (w) (throw 'quit 0)))

  (g-signal-connect button "clicked"
		    (lambda ()
		      (write standard-output "hello, world\n")))

  (gtk-container-add window button)
  (gtk-widget-show-all window)

  (setq interrupt-mode 'exit)
  (recursive-edit))

;; Local variables:
;; major-mode: lisp-mode
;; End:
