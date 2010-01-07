#! /bin/sh
exec rep --batch "$0" "$@"
!#

;;; rep-gtk TreeView example

(structure ()

    (open rep
	  rep.system
	  gui.gtk-2.gtk)

  ;; Widgets
  (define MainWindow (gtk-window-new 'toplevel))
 ; (define ListStore (gtk-list-store-newv 1 G_TYPE_STRING))
 ; (define TreeView (gtk-tree-view-new-with-model ListStore))
   (define TreeView (gtk-tree-view-new))

  ;; Windowborder
  (gtk-container-set-border-width MainWindow 10)

  ;; Exit handling
  (g-signal-connect MainWindow "delete_event" (lambda (w) (throw 'quit 0)))

  ;; Pack Widgets
  (gtk-container-add MainWindow TreeView)

  ;; Show widgets
  (gtk-widget-show-all MainWindow)

  ;; Stuff
  (setq interrupt-mode 'exit)
  (recursive-edit))
