(defun c:TRIANG1()

(setq P1 (getpoint"\n Enter first point of Triangle:"))

(setq P2 (getpoint"\n Enter second point of Triangle:"))

(setq P3 (getpoint"\n Enter third point of Triangle:"))

(Command "LINE" P1 P2 P3 "C")

)