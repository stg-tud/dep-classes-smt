; Datatypes
(declare-datatype Class (Nat Zero Succ))
(declare-datatype Variable (V W X Y Z))
(declare-datatype Field (F G H))
(declare-datatype Path ((var (id Variable)) (pth (obj Path) (field Field))))

; Propositions
(declare-fun instOf (Path Class) Bool)
(declare-fun instBy (Path Class) Bool)
(declare-fun pathEq (Path Path) Bool)

; Functions
(define-fun-rec subst-path ((p1 Path) (x Variable) (p2 Path)) Path
  (ite (is-var p1)
       (ite (= x (id p1)) p2 p1)
       (pth (subst-path (obj p1) x p2) (field p1))))

; Rules
(assert (forall ((p Path)) (pathEq p p))) ; refl
;(assert (forall ((a Bool)) (=> a a))) ; ident   ident === true
(assert (forall ((a Bool) (p Path) (c Class)) (=> (=> a (instBy p c)) (=> a (instOf p c))))) ; class    TODO: is it sensible that a can be false?

; C-Subst
;(assert (forall
;          ((as Bool) (a Bool) (p1 Path) (p2 Path) (x Variable))
;          (=> (and
;                (=> as a_substxp1)
;                (=> as (pathEq p2 p1)))
;              (=> as a_substxp2))))
(assert (forall
          ((cs Bool) (q1 Path) (q2 Path) (p1 Path) (p2 Path) (x Variable))
          (=> (and
                (=> cs (pathEq (subst-path q1 x p1) (subst-path q2 x p1)))
                (=> cs (pathEq p2 p1)))
              (=> cs (pathEq (subst-path q1 x p2) (subst-path q2 x p2))))))

(assert (forall
          ((cs Bool) (q Path) (c Class) (p1 Path) (p2 Path) (x Variable))
          (=> (and
                (=> cs (instOf (subst-path q x p1) c))
                (=> cs (pathEq p2 p1)))
              (=> cs (instOf (subst-path q x p2) c)))))

(assert (forall
          ((cs Bool) (q Path) (c Class) (p1 Path) (p2 Path) (x Variable))
          (=> (and
                (=> cs (instBy (subst-path q x p1) c))
                (=> cs (pathEq p2 p1)))
              (=> cs (instBy (subst-path q x p2) c)))))

; C-Prog
;(assert (forall
;          ((bs Bool) (a Bool) (as Bool))
;          (=> (and (=> as a)
;                   (=> bs as_xp))
;              (=> bs a_xp))))


; TODO: add explicit patheq symmetry rule? doesnt seem to be needed
; TODO: check whether we can state substitution rule with less implications. (remove 'as')
; TODO: recheck c-class equiv with instBY → instOf
; TODO: look for different solver principles, that might be better suited for this type of problem?


; path symmetry/transitivity
;(assert (not (=> (pathEq (var Y) (var X)) ; unsat
;                 (pathEq (var X) (var Y)))))
;(assert (not (=> (and (pathEq (var X) (var Y)) (pathEq (var Y) (var Z)))  ; unsat
;                 (pathEq (var X) (var Z)))))
;(assert (not (=> (and (pathEq (var X) (var Y))   ; unsat
;                      (pathEq (var Y) (var Z))
;                      (pathEq (var V) (var W))
;                      (pathEq (var W) (var X)))
;                 (pathEq (var V) (var Z)))))
;(assert (not (forall ((p Path) (q Path))
;                      (=> (pathEq p q) (pathEq q p)))))
;(assert (exists ((p Path) (q Path))
;                      (not (=> (pathEq p q) (pathEq q p)))))

;(assert (not (=> (and (instBy (var X) Succ) (instBy (pth (var X) F) Zero))    ; unsat
;                 (instOf (var X) Succ))))
;(assert (not (=> (and (instBy (var X) Succ) (instBy (pth (var X) F) Zero))    ; unsat
;                 (instOf (pth (var X) F) Zero))))
;(assert (not (=> (and (instBy (var X) Succ) (pathEq (var X) (var Y)))         ; unsat
;                 (instOf (var Y) Succ))))
;(assert (not (=> (and (instBy (var X) Succ) (pathEq (var Y) (var X)))         ; unsat
;                 (instOf (var Y) Succ))))
;(assert (not (=> (and (instBy (var X) Succ)                                   ; unsat
;                      (pathEq (var X) (var Y)) (pathEq (var Y) (var Z)))
;                 (instOf (var Z) Succ))))
;(assert (not (=> (and (instBy (var X) Succ)                                   ; unsat
;                      (pathEq (var X) (var Y)) (pathEq (var Z) (var Y)))
;                 (instOf (var Z) Succ))))
(assert (not (=> (and (instBy (var V) Succ)                                   ; unsat
                      (pathEq (var X) (var Y))
                      (pathEq (var Y) (var Z))
                      (pathEq (var W) (var X))
                      (pathEq (var V) (var W)))
                 (instOf (var Z) Succ))))

(check-sat)
