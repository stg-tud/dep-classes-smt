(declare-datatype Variable (x y))
(declare-datatype Path (X Xf Y))

; subst p1 x p2 p3 === in p1 subst x with p2 is p3
(declare-fun subst (Path Variable Path Path) Bool)

(assert (subst X x X X))
(assert (subst X x Xf Xf))
(assert (subst X x Y Y))

(assert (subst X y X X))
(assert (subst X y Xf X))
(assert (subst X y Y X))

; negative cases
(assert (not (subst X x X Xf)))
(assert (not (subst X x X Y)))

(assert (subst Xf x X Xf))
;(assert (subst Xff x Xf ???))

(check-sat)
(get-model)
