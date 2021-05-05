(declare-datatype Variable (X Y Z))
(define-fun substitute ((path-p Variable) (var-x Variable) (path-q Variable)) Variable (ite (= var-x path-p) path-q path-p))
(define-fun-rec path-equivalence ((path-p Variable) (path-q Variable)) Bool
  (or
    (= path-p path-q)
    (and (= path-p X) (= path-q Y))
    (and (= path-p Y) (= path-q Z))
    (exists (
      (path-r Variable)
      (path-s Variable)
      (var-x Variable))
      (and
        (path-equivalence path-s path-r)
        (path-equivalence
          (substitute path-p var-x path-r)
          (substitute path-q var-x path-r))))
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
(assert (path-equivalence X Z))
(check-sat)
(get-model)
