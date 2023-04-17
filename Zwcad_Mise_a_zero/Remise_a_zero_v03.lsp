(defun c:zero_v03 ( / explode layouts )
 (alert
    (printList
      (vl-sort
	(countBy vla-get-objectName
		 (vla-get-ModelSpace
		   (vla-get-ActiveDocument
		     (vlax-get-acad-object)
		   )
		 )
	)
	(function (lambda (a b) (< (car a) (car b))))
      )
    )
  )
    (setq layouts (vla-get-layouts (vla-get-activedocument (vlax-get-acad-object)))
         explode t
   )

    (vl-load-com)
 (vlax-map-collection
   (vla-get-Blocks (vla-get-ActiveDocument (vlax-get-acad-object)))
   '(lambda (block)
      (and (wcmatch (vla-get-name block) "`*Model_Space,`*Paper_Space*")
           (vlax-map-collection
             block
             '(lambda (obj)
                (cond ((vlax-method-applicable-p obj 'Move)
                       (vla-Move obj (vlax-3d-point '(0 0 0)) (vlax-3d-point '(0 0 1e99)))
                       (vla-Move obj (vlax-3d-point '(0 0 0)) (vlax-3d-point '(0 0 -1e99)))
      
                      )
                )
              )
              
           )
      )
    )
 )


 (princ)
 (princ)

(alert "Toutes les coordonnees Z ainsi que les elevations ont ete mise à zero")

)
  (defun countBy (fun col / key pair lst)
    (vlax-for obj col
      (setq key	(fun obj)
	    lst (if (setq pair (assoc key lst))
		  (subst (cons key (1+ (cdr pair))) pair lst)
		  (cons (cons key 1) lst)
		)
      )
    )
  )
 (defun printList (lst)
    (apply
      'strcat
      (mapcar
	(function
	  (lambda (pair)
	    (strcat
	      (substr (car pair) 5)
	      " : "
	      (itoa (cdr pair))
	      "\n"
	    )
	  )
	)
	lst
      )
    )
  )
(vl-load-com) (princ)

 (defun c:FlatHatch (obj / rtd patname mark ss sset newobj)

    ;radians to degrees
    (defun rtd (radians)
       (/ (* radians 180.0) pi)
    ) ;end

    (cond 
      ((TestZNormal obj)
        (CheckRename (vlax-get obj 'Elevation) 0)
        (vlax-put obj 'Elevation 0.0)
      )
      ((TestXYNormal obj)
        (vla-delete obj)
        (setq renameflag T)
      )
  
      ((and
         (vlax-property-available-p obj 'HatchObjectType)
         (= 1 (vlax-get obj 'HatchObjectType))
        )
        (vlax-put obj 'HatchObjectType 0)
        (ProcessList (list obj))
      )

      ((and
     
         (>= (atoi (getvar "AcadVer")) 16)
         (or patlst (setq patlst (LstACADPAT)))
         (setq patname (vlax-get obj 'PatternName))
         (vl-position patname patlst)
         (setq mark (entlast))
         (not (command "._hatchedit" (vlax-vla-object->ename obj) "b" "p" "n"))
          (setq sset (SSAfterEnt mark))
           (not (command "zoom" "object" sset ""))
         (setvar "hpassoc" 1) 
         ;(setvar "hpassoc" 0)
         (not (command "._hatch" patname 
                (vlax-get obj 'PatternScale)
                (rtd (vlax-get obj 'PatternAngle)) "s" sset ""
              )
         )
         ;; Restore previous zoom.
         (not (command "zoom" "previous"))
         ;; Delete boundary objects here rather than later.
         (if sset
           (mapcar 'vla-delete (SSVLAList sset))
         )
         (setq newobj (vlax-ename->vla-object (entlast)))
         (eq "AcDbHatch" (vlax-get newobj 'ObjectName))
         ;; updates the hatch
         (not (vl-catch-all-error-p 
           (vl-catch-all-apply 'vlax-invoke 
             (list newobj 'Evaluate))))
       ) ;and
        (vlax-put newobj 'HatchStyle (vlax-get obj 'HatchStyle))
        (vlax-put newobj 'AssociativeHatch 0)
        (ApplyProps obj newobj)
      )
      (T (CommandExplode obj))
    ) ;cond
  ) ;end

