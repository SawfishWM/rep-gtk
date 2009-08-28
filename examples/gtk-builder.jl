;; gtk-builder.jl

(require 'gui.gtk-2.gtk)

(structure ()
    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  (define builder (gtk-builder-new))

  (condition-case data (progn 
			 (gtk-builder-add-from-file builder "gtk-builder-test.glade")
			 (define window (gtk-builder-get-object builder "window"))
			 (gtk-widget-show-all window)
			 (g-signal-connect (gtk-builder-get-object builder "button") "pressed" 
					   (lambda ()
					     (message "Button pressed")))

			 (gtk-builder-connect-signals builder)
			 (setq interrupt-mode 'exit)
			 (recursive-edit))
    (glib-error (message (cadr data)))))

;; Local variables:
;; major-mode: lisp-mode
;; End:
