#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(structure ()

    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  (define window (gtk-window-new 'toplevel))

  (define button (gtk-scale-button-new 'menu 0 100 0.5 '("gtk-ok" "" "undo")))

  (gtk-container-set-border-width window 10)

  (g-signal-connect window "delete_event" (lambda (w) (throw 'quit 0)))

  (gtk-container-add window button)
  (gtk-widget-show-all window)

  (setq interrupt-mode 'exit)
  (recursive-edit))

;; Local variables:
;; major-mode: lisp-mode
;; End:
