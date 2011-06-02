#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(structure ()

    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  (define window (gtk-window-new 'toplevel))

  ;; example for GtkScaleButton                             = 0%     = 100%   = 1% - 99%
  ;;(define button (gtk-scale-button-new 'menu 0 100 0.5 '("gtk-no" "gtk-yes" "gtk-refresh")))

  ;; example for GtkComboBoxText
  (define button (gtk-combo-box-text-new))
  (mapc (lambda (x) (gtk-combo-box-text-append-text button x)) '("111" "222" "333"))

  (define label (gtk-label-new "The selection is shown here"))
  (define box (gtk-vbox-new nil 0))

  (g-signal-connect button "changed" (lambda ()
				       (gtk-label-set-text label (gtk-combo-box-text-get-active-text button))))

  (gtk-container-set-border-width window 10)

  (g-signal-connect window "delete_event" (lambda (w) (throw 'quit 0)))

  (gtk-container-add window box)

  (gtk-box-pack-start box label)
  (gtk-box-pack-start box button)

  (gtk-widget-show-all window)

  (setq interrupt-mode 'exit)
  (recursive-edit))

;; Local variables:
;; major-mode: lisp-mode
;; End:
