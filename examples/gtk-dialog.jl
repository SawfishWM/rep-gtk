;;;; gtk-dialog.jl

(require 'gtk)

;; Each BUTTON is (TEXT . RETURNED-VALUE)
(defun gtk-dialog (message &rest buttons)
  (let
      ((window (gtk-window-new 'toplevel))
       (vbox (gtk-vbox-new nil 0))
       (label (gtk-label-new message))
       (bbox (gtk-hbutton-box-new)))
    (catch 'exit
      (unwind-protect
	  (progn
	    (gtk-container-border-width window 6)
	    (gtk-signal-connect window "delete_event"
				(lambda ()
				  (throw 'exit nil)))
	    (gtk-container-add window vbox)
	    (gtk-box-pack-start vbox label)
	    (gtk-box-pack-end vbox bbox)
	    (mapc (lambda (cell)
		    (let
			((button (gtk-button-new-with-label (car cell))))
		      (GTK-WIDGET-SET-FLAGS button '(can-default))
		      (gtk-box-pack-start bbox button nil nil)
		      (gtk-signal-connect button "clicked"
					  (lambda ()
					    (throw 'exit (cdr cell))))))
		  buttons)
	    (gtk-widget-show-all window)
	    (gtk-main))
	(gtk-widget-destroy window)
	;; If I don't do this, the window isn't unmapped..
	(while (> (gtk-events-pending) 0)
	  (gtk-main-iteration))))))

(defun yes-or-no-p (question)
  (gtk-dialog question '("Yes" . t) '("No" . nil)))

(defun y-or-n-p (q)
  (yes-or-no-p q))

(defun map-y-or-n-p (question inputs callback)
  (let
      ((all-t t))
    (when (eq 'all-t (catch 'map
		       (while inputs
			 (let*
			     ((q (if (stringp question)
				     (format nil question (car inputs))
				   (question (car inputs))))
			      (a (gtk-dialog q
					     '("Yes" . t) '("No" . nil)
					     '("Yes to all" . all-t)
					     '("Quit" . quit))))
			   (cond ((or (eq a 'all-t) (eq a 'quit))
				  (throw 'map a))
				 (a
				  (callback (car inputs)))
				 (t
				  (setq all-t nil)))
			   (setq inputs (cdr inputs))))))
      ;; User answered with "!", so loop over all remaining inputs
      (while inputs
	(callback (car inputs))
	(setq inputs (cdr inputs))))
    all-t))
