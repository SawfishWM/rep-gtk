#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;;; rep-gtk hello world program 

(require 'gui.gtk-2.gtk)

(let* ((window (gtk-window-new 'toplevel))
       (view (gtk-text-view-new))
       (buffer (gtk-text-view-get-buffer view)))
  (gtk-text-buffer-set-text buffer "Hello, this is some text" -1)
  (gtk-signal-connect window "delete_event" (lambda () (throw 'quit 0)))
  (gtk-container-add window view)
  (gtk-window-set-default-size window 300 200)
  (gtk-widget-show-all window)

  (setq interrupt-mode 'exit)
  (recursive-edit))

;; Local variables:
;; major-mode: lisp-mode
;; End:
