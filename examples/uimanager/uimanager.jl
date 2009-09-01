;; Example from Foundations of GTK+ Development ported to rep-gtk

(require 'gui.gtk-2.gtk)

 
(defun main()
  (setq uimanager (gtk-ui-manager-new))
  (setq window (gtk-window-new 'toplevel))
  (gtk-window-set-title window "UI Manager")
  (gtk-widget-set-size-request window 250 -1)

  ;; Create a new action group and add all of the actions to it.
  (setq group (gtk-action-group-new "MainActionGroup"))
  ; FIXME: unimplemented  gtk-action-group-add-actions group entries NUM_ENTRIES, NULL)
  
  (let loop ((actions '(("File" "_File" nil nil)
			("Open" "Open" "Open an existing file" "gtk-open")
			("Save" "Save" "Save the document to a file" "gtk-save")
			("Quit" "Quit" "Quit the application" "gtk-quit")
			("Edit" "_Edit")			
			("Cut" "Cut" "Cut the selection to the clipboard" "gtk-cut")
			("Copy" "Copy" "Copy the selection to the clipboard" "gtk-copy")			
			("Paste" "Paste" "Paste text from the clipboard", "gtk-paste")
			("SelectAll" "SelectAll" "Select all of the text" "gtk-select-all" )
			("Deselect" "_Deselect", "Deselect all of the text" nil "<control>d")
			("Help" "_Help")
			("Contents"  "Get help on using the application" "gtk-help")
			( "About" "Acout" "More information about the application"))))
       (let* ((action-params (car actions))
	      (accel-param (nth 4 action-params)))
	 (when action-params
	   (if accel-param 
	       (gtk-action-group-add-action-with-accel group (apply gtk-action-new action-params) accel-param)
	     (gtk-action-group-add-action group (apply gtk-action-new action-params)))
	   (loop (cdr actions)))))
    

  (setq uimanager (gtk-ui-manager-new))
  (gtk-ui-manager-insert-action-group uimanager group 0)
  
  (gtk-ui-manager-add-ui-from-file uimanager "menu.ui")
  (gtk-ui-manager-add-ui-from-file uimanager "toolbar.ui")
  
  ; Retrieve the necessary widgets and associate accelerators. 
  (setq menubar (gtk-ui-manager-get-widget uimanager "/MenuBar"))
  (setq toolbar (gtk-ui-manager-get-widget uimanager "/Toolbar"))

  (gtk-toolbar-set-style toolbar  'icons)
  (gtk-window-add-accel-group window (gtk-ui-manager-get-accel-group uimanager))
  
  (setq vbox (gtk-vbox-new t 0))
  (gtk-box-pack-start-defaults vbox menubar)
  (gtk-box-pack-start-defaults vbox toolbar)

  (gtk-container-add window vbox)
  (gtk-widget-show-all window)
  (setq interrupt-mode 'exit)
  (recursive-edit))


(main)