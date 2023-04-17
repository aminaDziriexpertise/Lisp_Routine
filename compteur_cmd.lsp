(defun count-command-in-drawing (command-name csv-file)
  ; Initialiser le compteur de commande
  (setq command-counter 0)

  ; Boucler sur toutes les entités du dessin
  (foreach entity (entget (entlast))
    ; Vérifier si l'entité contient la commande
    (if (member command-name (cdr (assoc 0 entity)))
      ; Si la commande est trouvée, incrémenter le compteur
      (setq command-counter (1+ command-counter))
    )
  )

  ; Ouvrir le fichier CSV en écriture
  (setq csv-file-handle (open csv-file "w"))

  ; Écrire le nombre de fois que la commande a été exécutée dans le fichier CSV
  (write-line (strcat command-name "," (itoa command-counter)) csv-file-handle)

  ; Fermer le fichier CSV
  (close csv-file-handle)
)

; Exemple d'utilisation : compter le nombre de fois que la commande "line" a été exécutée
(count-command-in-drawing "LINE" "C:\\command-count.csv")
