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
  (define ListStore (gtk-list-store-newv 1 '(GdkPixbuf) 2 '(gchararray)))
  (define TreeView (gtk-tree-view-new-with-model ListStore))

  (define PixbufRender (gtk-cell-renderer-pixbuf-new))
  (define PixbufColumn (gtk-tree-view-column-new))
  (gtk-tree-view-column-pack-start PixbufColumn PixbufRender t)
  (gtk-tree-view-column-add-attribute PixbufColumn PixbufRender '("pixbuf") 0)
  (gtk-tree-view-append-column TreeView PixbufColumn)

  (define TextRender (gtk-cell-renderer-text-new))
  (define TextColumn (gtk-tree-view-column-new))
  (gtk-tree-view-column-pack-start TextColumn TextRender t)
  (gtk-tree-view-column-add-attribute TextColumn TextRender '("text") 1)
  (gtk-tree-view-append-column TreeView TextColumn)

  (define TestImage (gdk-pixbuf-new-from-file "test.png"))
  ;(gtk-list-store-append ListStore '(TestImage . "Test"))

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
