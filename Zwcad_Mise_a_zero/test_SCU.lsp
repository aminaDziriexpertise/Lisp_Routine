;création de la commande raccourci au clavier
(defun c:repscu ()

;cliquez le premier point
(setq P1 (getpoint "\ncliquer le point premier point du scu: "))

;cliquez le deuxieme point
(setq P2 (getpoint "\ncliquer le point deuxieme point du scu: "))

;création de la ligne de repere
(command "_line" P1 P2 "" )
(command "scu" "ob" (entlast))
(command "repere" "c")
(command "zoom" "o" (entlast) "")
;(command "effacer" (entlast) "")
);fin du dedun c:repscu


(defun c:twist ()
 (vl-cmdf "_.dview" "" "_twist" (getreal "\nSpécifiez la rotation: ") "")
 (princ)
)

(defun c:twist1 (/ h c)
 (setq h (getvar "viewsize"))
 (setq c (trans (getvar "viewctr") 1 0))
 (vl-cmdf "_.ucs" "z" pause)
 (vl-cmdf "_.plan" "_current")
 (vl-cmdf "_zoom" "_c" (trans c 0 1) h)
 (princ)
)

;;;*****************************************************************************
;;;*****************************************************************************
;;;************ 	   ** COMPARE 2 PLANS **		****************

;;;*****************************************************************************
;;;*****************************************************************************


(vl-load-com)

(defun c:cdwg (/       app     cdoc    fichier old     name    sp1
       sp2     lst2    e       obj     lstmodif	       lstajout
       lstsuppr	       lst     obj     sup
      )

(setq ACADLSPASDOC (getvar "ACADLSPASDOC"))
(setvar "ACADLSPASDOC" 0)

 (setq	app	(vlax-get-acad-object)
cdoc	(vla-get-activedocument app)
fichier	(getfiled
	  "Sélection de l'ancien plan"
	  (strcat (vl-filename-directory (vla-get-fullname cdoc))
		  "/"
	  )
	  "dwg"
	  4
	)
old	(vla-open (vla-get-documents app) fichier)
name	(vla-get-fullname cdoc)
s	(getvar "date")
t1	(* 86400.0 (- s (fix s)))
 )

 (vla-saveas
   cdoc
   (strcat (substr name 1 (- (strlen name) 4))
    " Modications depuis "
    (vla-get-name old)
   )
 )

 (setq	sp1 (vla-get-modelspace cdoc)
sp2 (vla-get-modelspace old)
 )

 (vlax-for lay	(setq layers (vla-get-layers cdoc))
   (vlax-put lay 'color 253)
   (vla-put-lock lay :vlax-false)
 )

 (vlax-for lay	(vla-get-layers old)
   (vla-put-lock lay :vlax-false)
 )

 (vlax-for ent	sp2
   (setq lst2 (cons (cons (vla-get-handle ent) ent) lst2))
 )

 (defun equal-objects (obj1 obj2)
   (vl-every
     (function
(lambda	(p)
  (or (not (vlax-property-available-p obj1 p))
      (equal (vlax-get obj1 p)
	     (vlax-get obj2 p)
	     1e-9
      )
  )
)
     )
     '(ObjectName    Center	    Radius	  Coordinates
StartPoint    EndPoint	    StartAngle	  EndAngle
MajorAxis     RadiusRatio   TextString	  InsertionPoint
Width	      Height
      )
   )
 )

 (vlax-put (vla-add layers "0.Ajouts") 'color 172)
 (vlax-put (vla-add layers "0.Modications") 'color 10)
 (vlax-put (vla-add layers "0.Suppressions") 'color 92)

 (vlax-for ent	sp1
   (vla-put-color ent 256)

   (if	(setq e (assoc (vlax-get ent 'handle) lst2))
     ;;vérifie si le handle existe sur les 2 plans
     (progn
(cond ((not (equal-objects ent (cdr e)))
       (vla-put-layer ent "0.Modications")
      )
)
(setq lst2 (vl-remove e lst2))
     )
     (vla-put-layer ent "0.Ajouts")
   )
 )

 (if lst2
   (foreach n lst2
     (vla-put-layer
(car (vlax-invoke old 'CopyObjects (list (cdr n)) sp1))
"0.Suppressions"
     )
   )
 )

 (vla-close old)
 (vla-purgeall cdoc)
 (vla-save cdoc)
 (setq	s  (getvar "date")
t2 (* 86400.0 (- s (fix s)))
tt (- t2 t1)
 )

 (if (> tt 60)
   (progn
     (setq sec	(rem tt 60)
    mn	(strcat (rtos (/ (- tt sec) 60) 2 0) " mn ")
    sec	(strcat (rtos sec 2 0) " s")
     )
   )
   (progn
     (setq sec	(strcat (rtos tt 2 0) " s")
    mn	"0 mn "
     )
   )
 )

 (alert
   (strcat
     "Plan de repérages des modifications\n\nLes ajouts sont en vert\nLes modifications en rouge\nLes suppressions en bleu\n\nTemps d'éxecution : "
     mn
     sec
   )
 )
(setvar "ACADLSPASDOC" ACADLSPASDOC)
)					;defun