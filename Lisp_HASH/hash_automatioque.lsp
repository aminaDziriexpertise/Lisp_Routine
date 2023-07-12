
(defun c:test (/ AcDoc Space nw_obj ename ent_text dxf_ent key)
  
  
  (vlax-put
		(vlax-ename->vla-object (entlast))
		'TextString
		(strcat
			"{\\fArial|b0|i0|c0|p34;"
			"%<\\AcObjProp.16.2 Object(%<\\_ObjId "
			(itoa (vla-get-ObjectID ename))
			">%).Area \\f \"%lu2%pr2%ps[S=,"
			"m²]\">%"
		)
	)
  
  ;;;;;;;;;;;;;;; Modifier style du text ;;;;;;;;;;;;;;;;;
  (if (tblsearch "Style" "IS_SIMPLEX")
 (command "_textstyle" "IS_SIMPLEX")
 (progn
  (command "_-style"
	"TEMP-STYLE" "simplex.shx" "" "0.7" "" "" "" "")
  (command "_rename" "_S" "TEMP-STYLE" "IS_SIMPLEX")
  (command "_textstyle" "IS_SIMPLEX")
  );progn
   );if
  

  
(if (=  (tblsearch "layer" "GEX_EDS_sdp_teinte_contour") nil)(command "-layer" "n" "GEX_EDS_sdp_teinte_contour" "CO" "U" "255,127,127" "GEX_EDS_sdp_teinte_contour" ""))*
; affecter geler sur le calque de contours dans le but de faire hachure en traitant nu intérieur
(command "-layer" "E" 0 "g"  "GEX_EDS_sdp_1-habitation"  "")""
(command "-hachures" "p" "s" "a" "s" "n" "a" "o" "i" "o" "h" "o" "" "")
(command "_clayer" "GEX_EDS_sdp_teinte_contour")
  
  GEX_EDS_sdp_teinte_contour

 ;(command "_.bhatch" "_properties" "solid" "_draw" "_back" pause "")
 ;(setvar "clayer" cc)
 (princ)
) 


(defun c:TestHach ()
 (setq n 0)
 (if (setq Select
            (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_1-habitation") (70 . 1)))
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
   (command "ordretrace" Select "" "ar" "")
 ) ;_ Fin de if

 (princ)
)


(defun c:multi_hatch-90 ( / js n model_hatch scale_hatch ang_hatch ent dxf_ent col lst_pt lst_d where alpha dxf_hatch)
 (setq js (ssget '((0 . "LWPOLYLINE") (-4 . "<AND") (-4 . "&") (70 . 1) (-4 . "AND>"))))
 (cond
   (js
     (setq
       n -1
       model_hatch (getvar "HPNAME")
       scale_hatch (getvar "HPSCALE")
       ang_hatch (getvar "HPANG")
     )
     (setvar "HPNAME" "SOLID")
     (setvar "HPSCALE" (* (getvar "HPSCALE") (getvar "DIMSCALE")))
     (repeat (sslength js)
       (setq
         ent (ssname js (setq n (1+ n)))
         dxf_ent (entget ent)
         col (if (assoc 62 dxf_ent) (cdr (assoc 62 dxf_ent)) 256)
         lst_pt (mapcar '(lambda (x) (trans x ent 0)) (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 10)) dxf_ent)))
         lst_d (mapcar 'distance lst_pt (cons (last lst_pt) lst_pt))
         where (- (length lst_pt) (length (member (apply 'max lst_d) lst_d)))
         alpha (angle (if (zerop where) (last lst_pt) (nth (1- where) lst_pt)) (nth where lst_pt))
       )
       (setvar "HPANG" (angle (trans '(0 0 0) 0 1) (trans (polar '(0 0 0) alpha scale_hatch) 0 1)))
       (command "_.HATCH" "" ent "")
       (if (assoc 62 (setq dxf_hatch (entget (entlast))))
         (entmod (subst (cons 62 col) (assoc 62 dxf_hatch) dxf_hatch))
         (entmod (append dxf_hatch (list (cons 62 col))))
       )
     )
     (setvar "HPNAME" model_hatch)
     (setvar "HPSCALE" scale_hatch)
     (setvar "HPANG" ang_hatch)
   )
   (T (princ "\nAucune LWPOLYLINE fermée trouvé !"))
 )
 (princ)
)






