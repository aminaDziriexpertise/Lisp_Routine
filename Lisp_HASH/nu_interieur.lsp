(defun c:nu_int_habitation ()
 (setq n 0)
 (if (setq Select
            (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_1-habitation") (70 . 1))) ;(70 . 1) ; drapeau d'état (gelé, dévérouillé...)
     ) ;_ Fin de setq
   (while (setq ent (ssname Select n))
     (setq elst (entget ent)
           n    (1+ n)
     ) ;_ Fin de setq
     (if (not (tblsearch "layer" "GEX_EDS_sdp_1-habitation_Hach"))
       (command "-calque" "n" "GEX_EDS_sdp_1-habitation_Hach" "")
       (setvar "clayer" "GEX_EDS_sdp_1-habitation_Hach")
     ) ;_ Fin de if
     (command "_-hatch" "p" "s" 1.00 0.00 "s" ent "" "")
   ) ;_ Fin de while
 ) ;_ Fin de if
 (if (setq Select (ssget "_X" '((0 . "HATCH")))) ;_ Fin de setq
   (command "ordretrace" Select "" "ar" "") ; D:devant
 ) ;_ Fin de if

 (princ)
)

(defun c:nu_int_bureau ()
 (setq n 0)
 (if (setq Select
            (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_1-bureau") (70 . 1))) ;(70 . 1) ; drapeau d'état (gelé, dévérouillé...)
     ) ;_ Fin de setq
   (while (setq ent (ssname Select n))
     (setq elst (entget ent)
           n    (1+ n)
     ) ;_ Fin de setq
     (if (not (tblsearch "layer" "GEX_EDS_sdp_1-bureau_Hach"))
       (command "-calque" "n" "GEX_EDS_sdp_1-bureau_Hach" "")
       (setvar "clayer" "GEX_EDS_sdp_1-bureau_Hach")
     ) ;_ Fin de if
     (command "_-hatch" "p" "s" 1.00 0.00 "s" ent "" "")
   ) ;_ Fin de while
 ) ;_ Fin de if
 (if (setq Select (ssget "_X" '((0 . "HATCH")))) ;_ Fin de setq
   (command "ordretrace" Select "" "ar" "") ; D:devant
 ) ;_ Fin de if

 (princ)
)

(defun c:nu_int_SDP ()
 (setq n 0)
 (if (setq Select
            (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_1-SDP") (70 . 1))) ;(70 . 1) ; drapeau d'état (gelé, dévérouillé...)
     ) ;_ Fin de setq
   (while (setq ent (ssname Select n))
     (setq elst (entget ent)
           n    (1+ n)
     ) ;_ Fin de setq
     (if (not (tblsearch "layer" "GEX_EDS_sdp_1-SDP_Hach"))
       (command "-calque" "n" "GEX_EDS_sdp_1-SDP_Hach" "")
       (setvar "clayer" "GEX_EDS_sdp_1-SDP_Hach")
     ) ;_ Fin de if
     (command "_-hatch" "p" "s" 1.00 0.00 "s" ent "" "")
   ) ;_ Fin de while
 ) ;_ Fin de if
 ;(if (setq Select (ssget "_X" '((0 . "HATCH")))) ;_ Fin de setq
  ; (command "ordretrace" Select "" "ar" "") ; D:devant
 ;) ;_ Fin de if

 (princ)
)
;; 
;; PolyArea dediee pour calcul de polyligne suite au choix de contour(ou objet) convenable
;; 


(defun c:PolyArea (/ *error* filter ss acSpace minPt maxPt pt id str txt)
 (vl-load-com)
 (or *acdoc* (setq *acdoc* (vla-get-ActiveDocument (vlax-get-acad-object))))
 (or *util* (setq *util* (vla-get-Utility *acdoc*)))
 (defun *error* (msg)
   (and msg
 (/= msg "Fonction annulée")
 (/= msg "Function cancelled")
 (princ (strcat "Erreur: " msg))
   )
   (vla-EndUndomark *acdoc*)
   (princ)
 )

 ;; entités fermées
 ;|
 (setq	filter (list
	 '(-4 . "<OR")
	 '(0 . "CIRCLE,MPOLYGON,REGION")
	 '(-4 . "<AND")
	 '(0 . "ELLIPSE")
	 '(41 . 0.0)
	 (cons 42 (* 2 pi))
	 '(-4 . "AND>")
	 '(-4 . "<AND")
	 '(0 . "LWPOLYLINE")
	 '(-4 . "&")
	 '(70 . 1)
	 '(-4 . "AND>")
	 '(-4 . "<AND")
	 '(0 . "POLYLINE")
	 '(-4 . "&")
	 '(70 . 1)
	 '(-4 . "<NOT")
	 '(-4 . "&")
	 '(70 . 120)
	 '(-4 . "NOT>")
	 '(-4 . "AND>")
	 '(-4 . "<AND")
	 '(0 . "SPLINE")
	 '(-4 . "&")
	 '(70 . 9)
	 '(-4 . "AND>")
	 '(-4 . "OR>")
       )
 )
 |;

 ;; entités fermées ou ouvertes
 (setq	filter (list
	 '(-4 . "<OR")
	 '(0 . "ARC,CIRCLE,LWPOLYLINE,MPOLYGON,REGION,ELLIPSE")
	 '(-4 . "<AND")
	 '(0 . "POLYLINE")
	 '(-4 . "<NOT")
	 '(-4 . "&")
	 '(70 . 120)
	 '(-4 . "NOT>")
	 '(-4 . "AND>")
	 '(-4 . "<AND")
	 '(0 . "SPLINE")
	 '(-4 . "&")
	 '(70 . 8)
	 '(-4 . "AND>")
	 '(-4 . "OR>"))
 ) 


 (prompt "Hauteur courante de MTEXT - Voir la Variable System TEXTSIZE ... \n"  ) 

 (prompt "\nPOLYAREA traite: ARC,CIRCLE,ELLIPSE,POLYLINE 2D,ELLIPSE,SPLINE,MPOLYGON,REGION \nSelectionnez les objets ou Entree pour tous ") 

 (if (or (ssget filter)
  (ssget "_X" (cons (cons 410 (getvar 'ctab)) filter))
     )
   (progn
     (vla-StartUndomark *acdoc*)
     (setq acSpace (vla-get-Block (vla-get-ActiveLayout *acdoc*)))
     (vlax-for	obj (setq ss (vla-get-ActiveSelectionSet *acdoc*))
(vla-GetBoundingBox obj 'minPt 'maxPt)
(setq pt  (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.))
		  (vlax-safearray->list minPt)
		  (vlax-safearray->list maxPt)
	  )
      id  (vla-GetObjectIdString *util* obj :vlax-false)
      str (strcat
	    "%<\\AcObjProp Object(%<\\_ObjId "
	    id
	    ">%).Area \\f \"%lu2%pr1%ps[,m²]%\">%"
	  )
      txt (vla-addMText acSpace (vlax-3d-point pt) 0. str)
)
(vla-put-AttachmentPoint txt acAttachmentPointMiddleCenter)
(vla-put-insertionPoint txt (vlax-3d-point pt))
     )
     (vla-Delete ss)
   )
 )
 (*error* nil)
) 