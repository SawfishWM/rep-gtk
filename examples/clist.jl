#! /bin/sh
exec rep --batch "$0" "$@"
!#

(require 'gtk)

(defvar titles ["name" "uid" "gid" "passwd" "gecos" "home" "shell"])

(defvar window (gtk-window-new 'toplevel))
(defvar scrolled-window (gtk-scrolled-window-new))
(defvar clist (gtk-clist-new-with-titles titles))

(gtk-container-add window scrolled-window)
(gtk-container-add scrolled-window clist)

(gtk-scrolled-window-set-policy scrolled-window 'automatic 'automatic)

(gtk-widget-set-usize window 400 200)
(gtk-signal-connect window "delete_event" #'(lambda () (gtk-exit)))

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

(gtk-widget-show-all window)
(gtk-main)
