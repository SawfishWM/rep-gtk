#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(require 'gui.gtk-2.gtk)

(let
    ((window (gtk-window-new 'toplevel))
     (button (gtk-button-new-with-label "say hello")))
  (gtk-container-set-border-width window 10)
  (g-signal-connect
   window "delete_event"
   #'(lambda (w) (if (gtk-standalone-p)
		     (throw 'quit 0)
		   (gtk-widget-destroy w))))
  (g-signal-connect
   button "clicked"
   #'(lambda () (if (featurep 'jade)
		    (insert "hello, world\n")
		  (write standard-output "hello, world\n"))))
  (gtk-container-add window button)
  (gtk-widget-show-all window)
  (when (gtk-standalone-p)
    (gtk-main)))

;; Local variables:
;; major-mode: lisp-mode
;; End:
