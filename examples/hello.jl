#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(require 'gtk)

(let
    ((window (gtk-window-new 'toplevel))
     (button (gtk-button-new-with-label "say hello")))
  (gtk-container-border-width window 10)
  (gtk-signal-connect
   window "delete_event"
   #'(lambda (w) (if (gtk-standalone-p)
		     (gtk-exit)
		   (gtk-widget-destroy w))))
  (gtk-signal-connect
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
