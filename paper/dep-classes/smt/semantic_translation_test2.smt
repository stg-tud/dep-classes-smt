(declare-datatype Class (Nat Zero Succ))
(declare-datatype Path (X Xf Y Yf))
(declare-fun instOf (Path Class) Bool)
(declare-fun instBy (Path Class) Bool)
(declare-fun pathEq (Path Path) Bool)

;(declare-const c1 Class)
;(declare-const c2 Class)
;(declare-const c3 Class)
;(declare-const c4 Class)
;(assert (distinct c1 c2 c3))
;(check-sat)
;(get-model)
;(assert (distinct c1 c2 c3 c4))
;(check-sat)

; refl
;(assert (forall ((p Path)) (=> true (pathEq p p))))
(assert (forall ((p Path)) (pathEq p p)))

; class
;(assert (forall ((p Path) (c Class)) (=> (instBy p c) (instOf p c))))  ; is this enought for the rule or should this incorporate the entailment?
(assert (forall ( (a Bool) (p Path) (c Class)) (=> (=> a (instBy p c)) (=> a (instOf p c))))) ; also unsat with above rule. Are this and the above rule semantically equivalent? No. For a=false, this reduces to true and not to instBy => instOf


;;;;;;;;;;;;;;;;;;;;;;;;;;;;(assert (forall ((p Path) (q Path)) (=> (distinct p q) (not (pathEq p q)))))

;(assert (not (=> (and (instOf X Nat) (instOf X Succ) (instOf Xf Zero)) (instOf X Succ))))
(assert (not (=> (and (instBy X Succ) (instBy Xf Zero)) (instOf X Succ))))

(check-sat)
