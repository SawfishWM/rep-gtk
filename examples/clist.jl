#! /bin/sh
exec rep --batch "$0" "$@"
!#

(require 'gtk)

(let*
    ((titles ["name" "uid" "gid" "passwd" "gecos" "home" "shell"])
     (window (gtk-window-new 'toplevel))
     (scrolled-window (gtk-scrolled-window-new))
     (clist (gtk-clist-new-with-titles titles)))

  (gtk-container-add window scrolled-window)
  (gtk-container-add scrolled-window clist)
  (gtk-scrolled-window-set-policy scrolled-window 'automatic 'automatic)
  (gtk-widget-set-usize window 400 200)
  (gtk-signal-connect
   window "delete_event"
   #'(lambda (w) (if (gtk-standalone-p)
		     (gtk-exit)
		   (gtk-widget-destroy w))))

  (let
      ((i 0))
    (while (< i 10)
      (gtk-clist-append clist
			(vector (format nil "record-%d" i)
				(format nil "field1-%d" i)
				(format nil "field2-%d" i)
				(format nil "field3-%d" i)
				(format nil "field4-%d" i)
				(format nil "field5-%d" i)
				(format nil "field6-%d" i)))
      (setq i (1+ i))))

  (let
      ((i 0))
    (while (< i (length titles))
      (gtk-clist-set-column-auto-resize clist i t)
      (setq i (1+ i))))

  (gtk-signal-connect clist "select_row" #'(lambda (&rest args)
					     (format standard-error
						     "select: %S\n" args)))

  (gtk-widget-show-all window)
  (when (gtk-standalone-p)
    (gtk-main)))

;; Local variables:
;; major-mode: lisp-mode
;; End:
