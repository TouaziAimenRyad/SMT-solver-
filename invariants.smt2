; synthèse d'invariant de programme
; on déclare le symbole non interprété de relation Invar
(declare-fun Invar (Int Int) Bool)

; la relation Invar est un invariant de boucle
(assert (forall (( x Int ) ( y Int )) (=> (and (Invar x y) (< x 3)) (Invar (+ x 1) (+ y 3)))))

; la relation Invar est vraie initialement
(assert (Invar 0 0))

; l'assertion finale est vérifiée
(assert (forall (( x Int ) ( y Int ))(=> (and (Invar x y) (>= x 3)) (= y 9))))

; appel au solveur
(check-sat-using (then qe smt))
(get-model)
(exit)