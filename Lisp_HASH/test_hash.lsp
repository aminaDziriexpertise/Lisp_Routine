(defun c:TestHach ()
 (setq n 0)
 (if (setq Select
            (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_teinte_contour")))
     ) ;_ Fin de setq
   (while (setq ent (ssname Select n))
     (setq elst (entget ent)
           n    (1+ n)
     ) ;_ Fin de setq
     (if (not (tblsearch "layer" "GEX_EDS_sdp_teinte_contour_Hach"))
       (command "-calque" "n" "GEX_EDS_sdp_teinte_contour_Hach" "")
       (setvar "clayer" "GEX_EDS_sdp_teinte_contour_Hach")
     ) ;_ Fin de if
     (command "_-hatch" "s" ent "" "")
   ) ;_ Fin de while
 ) ;_ Fin de if
 (if (setq Select (ssget "_X" '((0 . "HATCH")))) ;_ Fin de setq
   (command "ordretrace" Select "" "ar" "")
 ) ;_ Fin de if

 (princ)
)