(declare-datatype Variable (X Y Z))
(define-fun substitute ((path-p Variable) (var-x Variable) (path-q Variable)) Variable (ite (= var-x path-p) path-q path-p))
(define-fun equiv ((a Variable) (b Variable)) Bool
  (or
    (and (= a X) (= b Y))
    (and (= a Y) (= b Z))))
(define-fun path-equivalence ((path-a Variable) (path-b Variable)) Bool
  (or
    (= path-a path-b)
    (equiv path-a path-b)
    ;(equiv path-b path-a)
    (exists (
      (path-r Variable)
      (path-s Variable)
      (var-x Variable))
      (and
        (equiv path-s path-r)
        (equiv
          (substitute path-a var-x path-r)
          (substitute path-b var-x path-r))))
     ))
;(assert (forall ((path-p Variable)) (path-equivalence path-p path-p)))
;(assert (forall ((cs-a Bool) (path-p Variable) (path-q Variable) (path-r Variable) (path-s Variable) (var-x Variable))
;          (=>
;            (and
;              (=>
;                cs-a
;                (path-equivalence
;                  (substitute path-p var-x path-r)
;                  (substitute path-q var-x path-r)))
;              (=>
;                cs-a
;                (path-equivalence path-s path-r)))
;            (=>
;              cs-a
;              (path-equivalence
;                (substitute path-p var-x path-s)
;                (substitute path-q var-x path-s))))))
(assert (path-equivalence Y X))
(assert (path-equivalence X Z))
;(assert (path-equivalence Z X))
(check-sat)
(get-model)
