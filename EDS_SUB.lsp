;=====================================================
             Routine de calcul de la SUB
;=====================================================
(defun c:sub ()
	;(setq *echelle* (getstring "\n entrer echelle espace papier : ")) ; valeur par defaut si Nature vide == Locaux 
  ; traitement des caractéres spéciaux
  (setq é (strcat "\\U+00E9" ))
  (setq Reserves  (strcat "R" é "serves"))
  ;(alert Reserves)
  (setq AR  (strcat "Archives, " Reserves ))
  ;(alert AR)

  (setq RC  (strcat Reserves  " commerces"))
  ;(alert RC)
  

  (setq generale  (strcat "g" é "n"é"rale"))
  (setq CG  (strcat "Circulation " generale))
  ;(alert CG)
 
 
  (setq generaux  (strcat "g" é "n"é"raux"))
  (setq LTG  (strcat "Locaux techniques " generaux))
 ; (alert LTG)
 
	; variables globales 
	(setq *total_annexes* 0 *total_sub* 0 *ind* 0)
	(setq *ech* 1 *xrec* 32)
	(setq *xaf* 33 *xsurf* 40 *y* 19 *a* -0.5 *surfT* 0)

	; d�finit des variables qui serviront � l'affichage de la l�gende et au calcul des totaux de SUB
	;(setq total_annexes 0 total_sub 0 ind 0 ech 1 xrec 32 xaf 33 xsurf 40 y 19 a -0.5 surfT 0)
	(setq xysurf (list *xsurf* *y*))
	(setq xyaf (list *xaf* *y*))
	(setq xyrec (list *xrec* *y*))
	
	(if (=  (tblsearch "style" "swis BT roman") nil)								;recherche un style de texte 
		(command "_STYLE" "swis BT roman" "swiss.ttf" "0.000" "" "" "" "" "")
	)																				; si le style n'est pas pr�sent, creation de ce dernier 
		
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "HPGAPTOL" 0.0002)													;variable de gestion des espaces 
	(setvar "HPSEPARATE" 0)														; Un objet de hachures unique est cr��
	(setvar "HPASSOC" 0)														; associer la hachure � l'objet de contour
	;(setvar "HPISLANDDETECTION" 0)												; Hachure uniquement les zones en dehors des �lots.
	(setq ti (getvar "tilemode"))  (if (= ti 1) (command "_tilemode" "0"))		;command permettant de passer de l'espace papier � l'espace objet 
	
	(if (=  (tblsearch "layer" "GEX_MEP_ep") nil)(command "calque" "N" "GEX_MEP_ep" "CO" "U" "0,0,0" "GEX_MEP_ep" ""))
	
	; insertion des blocs pour la legende 
	(if (= (tblsearch "block" "EDS_SUB_LEG" nil)) (command "-inserer" "EDS_SUB_LEG" 0,0))				; Bloc pour chaque nature 
	(if (= (tblsearch "block" "EDS_SUB_TOT" nil)) (command "-inserer" "EDS_SUB_TOT" 0,0))				; bloc de total sub sans annexe
	(if (= (tblsearch "block" "EDS_SUB_TOT_AN" nil)) (command "-inserer" "EDS_SUB_TOT_AN" 0,0))			; bloc de total annexes sub
	(if (= (tblsearch "block" "EDS_SUB_AN_TOT" nil)) (command "-inserer" "EDS_SUB_AN_TOT" 0,0))			; bloc total sub avec annxes
	
	(setq ti (getvar "tilemode")) (if (= ti 0)(command "_tilemode" "1"))						; basculer sur l'espace objet on n'est pas dessus

	;=================================   BUREAU  ==============================
	(calculETaffiche "GEX_EDS_SU_bureau" "Bureaux" 0)

	;================================   HABITATION  ===========================
	(calculETaffiche "GEX_EDS_SU_habitation" "Habitation" 0)
	
	;================================   COMMERCE  =============================
	(calculETaffiche "GEX_EDS_SU_commerce" "Commerces" 0)

	;============================   COMMERCE RESERVES  ========================
	(calculETaffiche "GEX_EDS_SU_comm_reserv" RC 0)

	;===========================   Circulation privative  =====================
	(calculETaffiche "GEX_EDS_SU_circ_priv" "Circulation privative" 0)

	;==============================   Locaux sociaux  =========================
	(calculETaffiche "GEX_EDS_SU_LOC_SOCIAL" "Locaux sociaux" 0)

	;===============================   Locaux vides  ==========================
	(calculETaffiche "GEX_EDS_SU_loc_dispo" "Locaux vides" 0)

	;==================================   Archives  ===========================
	(calculETaffiche "GEX_EDS_SU_archives" AR 0)

	;==============================   Tablettes  ==============================
	(calculETaffiche "GEX_EDS_SU_tablette" "Tablettes" 0)

	;==============================   Circulation g�n�rale  ===================
	(calculETaffiche "GEX_EDS_SU_circ_gene" CG 0)

	;==============================   Sanitaires   ============================
	(calculETaffiche "GEX_EDS_SU_sanitaires" "Sanitaires" 0)

	;==============================   Palier  =================================
	(calculETaffiche "GEX_EDS_SU_palier" "Palier" 0)

	;==============================   Local entretien  ========================
	(calculETaffiche "GEX_EDS_SU_loc_entretien" "Locaux entretien" 0)

	;==============================   L.T.P.  =================================
	(calculETaffiche "GEX_EDS_SU_LTP" "Locaux techniques particuliers" 0)	

	;==============================   R.I.E.  =================================
	(calculETaffiche "GEX_EDS_SU_rie" "R.I.E." 0)

	;==============================   Annexe R.I.E.  ==========================
	(calculETaffiche "GEX_EDS_SU_rie_annexe" "Annexe R.I.E." 0)

	;==============================    DIVERS_01  =============================
	(calculETaffiche "GEX_EDS_SU_divers_01" "Divers 01" 0)
	
	;==============================    DIVERS_02  =============================
	(calculETaffiche "GEX_EDS_SU_divers_02" "Divers 02" 0)

	;==============================   DIVERS_03  ==============================
	(calculETaffiche "GEX_EDS_SU_divers_03" "Divers 03" 0)
	
	;==============================   DIVERS_04  ==============================
	(calculETaffiche "GEX_EDS_SU_divers_04" "Divers 04" 0)
	
	;==============================   DIVERS_03  ==============================
	(calculETaffiche "GEX_EDS_SU_divers_05" "Divers 05" 0)

	;==============================   TOTAL SUB  ==============================
	(command "_tilemode" "0")
	(setvar "clayer" "GEX_MEP_ep")
	(setq *total_sub* (rtos *total_sub* 2 1))
	(setvar "attdia" 0)
	
	;===> avant insertion de la l�gende tester s'il n'y a pas d'annexe sub
	(if (and (= (isemptyCalque "GEX_EDS_SU_LTG") 1)
			 (= (isemptyCalque "GEX_EDS_SU_pk") 1)
			 (= (isemptyCalque "GEX_EDS_SU_h-1.80") 1)
			 (= (isemptyCalque "GEX_EDS_SU_tablette_h-1.80") 1)
			 (= (isemptyCalque "GEX_EDS_su_surface_non_close") 1)
		)
			
		; pas d'annexe pr�sente dans le dessin ==> l�gende simple
		(progn
			(princ "----------- Pas d'annexes -------------------- \n")
			(command "-inserer" "EDS_SUB_TOT" xyrec "1" "1" "" "" "")
			(setvar "attdia" 1)
			(command "-attedit" "N" "N" "EDS_SUB_TOT" "SUB_TOTAL" "0" "0" *total_sub*)
			(command "_tilemode" "1")
			(setq *y* (- *y* 1.7))
		)
		; Annexe pr�sente dans le dessin ==> total avec legende annexe
		(progn 
			(princ "------------ Annexes presentes ----------------- \n")
			(command "-inserer" "EDS_SUB_TOT_AN" xyrec "1" "1" "" "" "")
			(setvar "attdia" 1)
			(command "-attedit" "N" "N" "EDS_SUB_TOT_AN" "SUB_TOTAL" "0" "0" *total_sub*)
			(command "_tilemode" "1")
			(setq *y* (- *y* 1.7))

			;; insertion de toutes les annexes 
			;==============================   Annexe LTG   ============================
			(calculETaffiche "GEX_EDS_SU_LTG" LTG 1)

			;==============================   Annexe Parking   ========================
			(calculETaffiche "GEX_EDS_SU_pk" "Parking" 1)

			;==============================   Annexe h<1.80m   ========================
			(calculETaffiche "GEX_EDS_SU_h-1.80" "Surfaces h < 1.80 m" 1)

			;==============================   Annexe tablette   =======================
			(calculETaffiche "GEX_EDS_SU_tablette_h-1.80" "Tablettes h < 1.80 m" 1)

			;==============================   Annexe Surfaces non closes  =============
			(calculETaffiche "GEX_EDS_su_surface_non_close" "Surfaces non closes" 1)
			

			;==============================   TOTAL ANNEXES  ==========================
			(setvar "clayer" "GEX_MEP_ep")
			(setq *total_annexes* (rtos *total_annexes* 2 1))
			(command "_tilemode" "0")
			(setvar "attdia" 0)
			
			;; insertion de la legende du total des surfaces annexes 
			(command "-inserer" "EDS_SUB_AN_TOT" xyrec "1" "1" "" "" "")
			(setvar "attdia" 1)
			(command "-attedit" "N" "N" "EDS_SUB_AN_TOT" "TOTAL_ANNEXES" "0" "0" *total_annexes*)
		)
	)
	
	(alert "                    ***  Operation terminee   ***                          ")
	(setvar "osmode" os)
	(princ)
)

;====== Fonctions utiles de calcul et d'affichage pour chaque nature ===========
(defun calculETaffiche (calque_nature texte annexe)   ;Calcul et affiche l'aire des surfaces de SUB
	(setq sel_poly (ssget "x" (list (cons 8 calque_nature) (cons 0 "LWPOLYLINE,CIRCLE"))))
	
	(if (/= sel_poly nil)
		(progn
			(princ calque_nature)
			(princ "\n")
			;definition des parametres pour positionner l'affichage des rectangle + texte + valeur de surface
			(setq xyrec (list *xrec* *y*)) 
			(setq xyrec2 (list (+ *xrec* 0.6) (+ *y* 0.3) )) 
			(setq xyaf (list *xaf* *y*)) 
			(setq xysurf (list *xsurf* *y*)) 
			(setq tx texte)				
			(setq surfT 0 ind 0)
			
			;calcul de la valeur de la surface par lecture du calque
			(setvar "clayer" calque_nature)									; parametrage le calque courant
			(command "-hachures" "_properties" "solid" "s" sel_poly "" "" )	; creation de la hachure sur le calque
			
			(command "_area" "_o" "_last")									;calcul de l'aire de la hachure 
			
			(setq surf (getvar "area"))
			(setq surfT (rtos surf 2 1))									;conversion de la valeur en chaine de caract�re
			(setq surfA (atof (rtos surf 2 1)))
			
			;insertion du bloc et de la valeur calculee 
			(command "_tilemode" "0")										;positionnement sur l'espace papier/presentation
			(setvar "clayer" "GEX_MEP_ep")									;parametrage le calque courant
			(setvar "attdia" 0)												;specifier que la commande suivante se fera sans boite de dialogue
			(command "-inserer" "EDS_SUB_LEG" xyrec "1" "1" "" "" "")			;insertion du bloc � la position xyrec 			
			(setvar "attdia" 1)
			(command "-attedit" "N" "N" "EDS_SUB_LEG" "DESIGNATION_SUB" "P" "P" tx )  ;modification du bloc ins�r� et ajout du texte descriptif
			(command "-attedit" "N" "N" "EDS_SUB_LEG" "SURFACE_SUB" "0" "0" surfT )	  ;modification du bloc et insertion de la valeur de surface calcul�e
			(command "_rectang" xyrec xyrec2)									  ;dessin du rectangle qui servira � identifier la 
			(setvar "clayer" calque_nature)
			(command "-hachures" "_properties" "solid" "s" "_last" "" "" )		  ;application de la teinte au rectangle de legende
			
			; incrementaion de la position et du total de surface
			(setq *y* (+ *y* *a*))
			(command "_tilemode" "1")										;positionnement sur l'espace objet
			
			(if (= annexe 0) (setq *total_sub* (+ *total_sub* surfA)))		;incr�ment de la somme cumul�e des surfaces sub
			(if (= annexe 1) (setq *total_annexes* (+ *total_annexes* surfA))) ;incr�ment de la somme cumul�e des annexes sub
		)	
	)	 ;Fin de IF et PROGN
)



;; ==== tester si calque est vide et ne contient rien ======
(defun isemptyCalque(calque)
	;; recuperation de toutes les polylignes du calque 
	(setq sel_poly (ssget "x" (list (cons 8 calque) (cons 0 "LWPOLYLINE,CIRCLE"))))
	
	(if (/= sel_poly nil)
		(princ 0) ;; si c'est non vide
		(princ 1) ;; si c'est vide
		)
)