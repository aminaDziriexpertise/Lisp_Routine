
(defun c:pentepc (/ p1 p2 a)
 (vl-load-com)
 
 (if
   (and
     (setq p1 (getpoint "\nPremier  point: "))
     (setq p2 (getpoint "\nDeuxieme point: "))
     (setq a (angle p1 p2))
   )
    (entmake
      (list
 '(0 . "TEXT")
 '(100 . "AcDbEntity")
 '(100 . "AcDbText")
 '(10 0. 0. 0.)
 (cons 40 (getvar 'textsize))
 (cons 1
       (strcat (rtos (abs  (/ (sin a) (cos a))) 2 2) )
 )

 (cons 50
       (if (minusp (cos a)) ;minusp: Verifies that a number is negative
	 (+ pi a)
	 a
       )
 )
 '(72 . 1)
 (cons 11 (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.)) p1 p2))
 '(73 . 1)
      )
    )
 )

 (princ)
)



; calcul de la pente en pourcentage
(defun c:pente%	(/ p1 p2 a)
 (if
   (and
     (setq p1 (getpoint "\nPremier point: "))
     (setq p2 (getpoint p1 "\nDeuxième point: "))
     (setq a (angle (setq p1 (trans p1 1 0)) (setq p2 (trans p2 1 0))))
   )
    (entmake
      (list
 '(0 . "TEXT")
 '(100 . "AcDbEntity")
 '(100 . "AcDbText")
 '(10 0. 0. 0.)
 (cons 40 (getvar 'textsize))
 (cons 1
       (strcat (rtos (abs (* 100 (/ (sin a) (cos a)))) 2 2) " %")
 )
 (cons 50
       (if (minusp (cos a))
	 (+ pi a)
	 a
       )
 )
 '(72 . 1)
 (cons 11 (mapcar '(lambda (x1 x2) (/ (+ x1 x2) 2.)) p1 p2))
 '(73 . 1)
      )
    )
 )
 (princ)
)