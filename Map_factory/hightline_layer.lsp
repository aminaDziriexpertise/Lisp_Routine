(defun flash-layer-contour ( duration / old-color new-color start-time)
  (setq old-color (cdr (assoc 62 (tblsearch "LAYER" "GEX_EDS_sdp_1-bureau"))))
  (setq new-color 1)
  (setq start-time (getvar 'tick))
  (while (< (- (getvar 'tick) start-time) duration)
    (entmod (list (cons 62 new-color)) (list (cons 8 "GEX_EDS_sdp_1-bureau")))
    (setq new-color (if (= new-color 1) old-color 1))
    (command "_.REGEN")
    (sleep 50)
  )
  (entmod (list (cons 62 old-color)) (list (cons 8 "GEX_EDS_sdp_1-bureau")))
  (command "_.REGEN")
)

; Exemple d'utilisation :
(defun highlight-polyline (obj duration / old-color new-color start-time)
  (setq old-color (cdr (assoc 62 (entget obj))))
  (setq new-color 1)
  (setq start-time (getvar 'tick))
  (while (< (- (getvar 'tick) start-time) duration)
    (entmod (list (cons 62 new-color)) obj)
    (setq new-color (if (= new-color 1) old-color 1))
    (command "_.REGEN")
    (sleep 50)
  )
  (entmod (list (cons 62 old-color)) obj)
  (command "_.REGEN")
)
(defun c:test_ligne_hight ()

; Exemple d'utilisation :
(setq pline (car (entsel "Sélectionnez la polyligne : ")))
(highlight-polyline pline 5000) ; met en surbrillance la polyligne pendant 5 secondes




)

(defun c:al-highlight ()

(vl-load-com)

(setq util (vla-get-utility 
                   (vla-get-activedocument 
                        (vlax-get-acad-object))))
                        
(vla-getentity util 'obj 'ip "\nSelect Object: ")

(vla-Highlight obj 21)

(princ)

);defun
(defun c:hlight  (/ lLst lay ObjLst grdat iEnt)
 (vl-load-com)
 (vlax-for l
              (vla-get-layers
                (vla-get-ActiveDocument
                  (vlax-get-acad-object)))
   (setq lLst (cons (vla-get-Name l) lLst)))
 (if (and (setq lay (strcase (getstring t "\nSpecify layer to Highlight: ")))
          (member lay (mapcar 'strcase lLst)))
   (progn
     (setq ObjLst (mapcar 'cadr (ssnamex (ssget "_X" (list (cons 8 lay))))))
     (princ "\nMove Cursor Over Objects....")
     (while (eq 5 (car (setq grdat (grread t 4 2))))
       (if (and (setq iEnt (car (nentselp (cadr grdat))))
                (eq lay (strcase (cdr (assoc 8 (entget iEnt))))))
         (mapcar '(lambda (x) (redraw x 3)) ObjLst)
         (mapcar '(lambda (x) (redraw x 4)) ObjLst))))
   (princ "\n<!> Layer not Found <!>"))
 (princ))


(defun c:hlight2  (/ lLst lay ObjLst NulLst grdat iEnt)
 (vl-load-com)
 (vlax-for l
              (vla-get-layers
                (vla-get-ActiveDocument
                  (vlax-get-acad-object)))
   (setq lLst (cons (vla-get-Name l) lLst)))
 (if (and (setq lay (strcase (getstring t "\nSpecify layer to Highlight: ")))
          (member lay (mapcar 'strcase lLst)))
   (progn
     (setq ObjLst (mapcar 'cadr (ssnamex (ssget "_X" (list (cons 8 lay)))))
           NulLst (mapcar 'cadr
                          (ssnamex (ssget "_X"
                                          (list (cons -4 "<NOT")
                                                (cons 8 lay)
                                                (cons -4 "NOT>"))))))
     (princ "\nMove Cursor Over Objects....")
     (while (eq 5 (car (setq grdat (grread t 4 2))))
       (if (and (setq iEnt (car (nentselp (cadr grdat))))
                (eq lay (strcase (cdr (assoc 8 (entget iEnt))))))
         (progn
           (mapcar '(lambda (x) (redraw x 3)) ObjLst)
           (mapcar '(lambda (x) (redraw x 2)) NulLst))
         (progn
           (mapcar '(lambda (x) (redraw x 4)) ObjLst)
           (mapcar '(lambda (x) (redraw x 1)) NulLst)))))
   (princ "\n<!> Layer not Found <!>"))
 (princ))
(defun c:hlight3  (/ lay ObjLst NulLst grdat iEnt)
 (princ "\nMove Cursor Over Objects....")
 (while (eq 5 (car (setq grdat (grread t 4 2))))
   (if (setq iEnt (car (nentselp (cadr grdat))))
     (progn
       (setq lay (strcase (cdr (assoc 8 (entget iEnt)))))
       (setq ObjLst (mapcar 'cadr (ssnamex (ssget "_X" (list (cons 8 lay)))))
             NulLst (mapcar 'cadr
                            (ssnamex (ssget "_X"
                                            (list (cons -4 "<NOT")
                                                  (cons 8 lay)
                                                  (cons -4 "NOT>"))))))
       (mapcar '(lambda (x) (redraw x 3)) ObjLst)
       (mapcar '(lambda (x) (redraw x 2)) NulLst))
     (if (and ObjLst NulLst)
       (progn
         (mapcar '(lambda (x) (redraw x 4)) ObjLst)
         (mapcar '(lambda (x) (redraw x 1)) NulLst)))))
 (princ))



(defun c:layer_hight ()
(command"_ZOOM""_E")

	;zooms to extents
(command"_HATCHTOBACK")
	;sends all hatching to back of drawing order

 
(if (setq tss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "GEX_EDS_sdp_1-habitation") (70 . 1))))
(repeat (setq n (sslength tss))
(setq tdata (entget (ssname tss (setq n (1- n)))))
(entmod (subst '(7 . "LEGEND") (assoc 7 tdata) tdata))
); repeat
); if

(mapcar '(lambda (x) (vla-put-constantwidth x 1)) (mapcar 'vlax-ename->vla-object (mapcar 'cadr (ssnamex (ssget "_x" '((0 . "*POLYLINE")))))))
   (princ)

 ;( restart)

)
(defun c:restart()
 (command "DELAY" 1000) 
 (command "Annuler" "150"))