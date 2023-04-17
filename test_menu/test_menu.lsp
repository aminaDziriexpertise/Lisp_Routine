(vl-load-com)
(defun c:Example_OnMenuBar()
   ;; This example creates a new menu called TestMenu and inserts a menu item
   ;; into it. The menu is then displayed on the menu bar, and then
   ;; removed from the menu bar.
   (command "_menuload" "C:\\Users\\adziri\\OneDrive - GEXPERTISE SYSTEMS\\Bureau\\menu_lisp\\test_menu\\test.cuix")
   (setq acadObj (vlax-get-acad-object))
   (setq currMenuGroup (vla-Item (vla-get-MenuGroups acadObj) 0))
   
   ;; Create the new menu
   (setq newMenu (vla-Add (vla-get-Menus currMenuGroup) "&GexCalq.v5"))
   
   ;; Add a menu item to the new menu
   ;; Assign the macro string the VB equivalent of "ESC ESC _open "
   (setq openMacro (strcat (Chr 3) (Chr 3) (Chr 95) "open" (Chr 32)))
   (setq newMenuItem (vla-AddMenuItem newMenu (1+ (vla-get-Count newMenu)) "Open" openMacro))

    ;Calques BAT
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "BATI" openmacro ))
    (vla-item (vla-item (vla-getextensionDictionary
     (vla-get-Layers(vla-get-ActiveDocument (vlax-get-acad-object)))
     )
   "ACAD_LAYERFILTERS")"GEX_EDS_sdp") 
  
    (vla-put-helpstring newmenuitem "Charger les calques pour BATI")
   ;Calques EDS
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "EDS" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques pour EDS")
    ;Calques TOPO
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32) ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "TOPO" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques pour TOPO")
    ;Calques RZO
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32) ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "RZO" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques pour RZO")
    ;Calques NFP
    (setq openmacro (strcat (chr 3) (chr 3)  (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "NFP" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques pour NFP")
    ;Calques MEP
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "MEP" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques de mise en page")
    ;Calques FON
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "FON" openmacro ))
    (vla-put-helpstring newmenuitem "Charger les calques du foncier")
    ;Calques TOUS
    (setq openmacro (strcat (chr 3) (chr 3) (Chr 95) "CalqInit" (chr 32)  ))
    (setq newmenuitem (vla-addmenuitem newmenu (1+ (vla-get-count newmenu)) "TOUS" openmacro ))
    (vla-put-helpstring newmenuitem "Charger tous les calques")


   ;; Display the menu on the menu bar
   (vla-InsertInMenuBar newMenu (1+ (vla-get-Count (vla-get-MenuBar acadObj))))
   (if (= (vla-get-OnMenuBar newMenu) :vlax-true)
       (alert (strcat "The menu called " (vla-get-Name newMenu) " is on the menu bar."))
       (alert (strcat "The menu called " (vla-get-Name newMenu) " is not on the menu bar."))
   )
   
  
)
(c:Example_OnMenuBar)