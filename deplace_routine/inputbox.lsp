(defun c:deplace0( / distance_deplacement p0 p1 index_objet_a_deplacer jeu objet)
  ;(command "_.-purge" "_all" "*" "_yes")
  ;(command "_.ZOOM" "_extent")
  
  (setq dcl_id (load_dialog "inputbox.dcl"))
  (if (not (new_dialog "inputbox" dcl_id))
    (exit)
  )
 ; (alert test((rtos distance_deplacement 2 0)))
  ;(setq distance_deplacement  (get_tile"eb1"))

  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)"
  )
  (action_tile
    "accept"
    ;"(done_dialog) (setq result distance_deplacement)"
    "(setq distance_deplacement (get_tile \"eb1\"))
     (done_dialog)
    (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
  (initget 7)
  ;(setq distance_deplacement (getreal "\nInserer la distance adequate des objets: "))
  (setq p0 '(0.0 0.0))
  (setq p1 (list distance_deplacement 0.0))
  (setq index_objet_a_deplacer 0)
  (setq jeu (ssget))
  (repeat (sslength jeu)
	(setq objet (ssname jeu index_objet_a_deplacer))
  	(command "_.move" objet "" p0 p1)
  	(setq index_objet_a_deplacer (1+ index_objet_a_deplacer))
	(setq p1 (list (+ (car p1) distance_deplacement) 0.0))
   )
   (prin1)
)




(defun inputbox ()

  (setq dcl_id (load_dialog "inputbox.dcl"))
  (if (not (new_dialog "inputbox" dcl_id))
    (exit)
  )
 ; (alert test((rtos distance_deplacement 2 0)))
  ;(setq distance_deplacement  (get_tile"eb1"))

  (action_tile
    "cancel"
    "(done_dialog)
     (setq result nil)"
  )
  (action_tile
    "accept"
    ;"(done_dialog) (setq result distance_deplacement)"
    "(setq distance_deplacement (get_tile \"eb1\"))
     (done_dialog)
    (setq result T)"
  )
  (start_dialog)
  (unload_dialog dcl_id)
  (princ)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ)
