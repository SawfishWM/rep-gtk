#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(require 'gtk)

(setq window (gtk-window-new 'toplevel))
(gtk-container-border-width window 10)
(gtk-signal-connect window "delete_event" #'(lambda () (gtk-exit)))

(setq button (gtk-button-new-with-label "hello, world"))
(gtk-signal-connect button "clicked"
		    #'(lambda () (write standard-output "hello, world\n")))
(gtk-container-add window button)
(gtk-widget-show button)

(gtk-widget-show window)

(gtk-main)

