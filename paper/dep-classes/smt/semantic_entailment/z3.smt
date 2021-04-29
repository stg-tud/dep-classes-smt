(declare-datatype Variable (a b c d e f g h i j k))
;(declare-datatype Variable (a b c d e f g h i j k l m n o p q r s t u v w x y z))
(declare-fun path-equivalence (Variable Variable) Bool)
(define-fun substitute ((path-p Variable) (var-x Variable) (path-q Variable)) Variable (ite (= var-x path-p) path-q path-p))
(assert (forall ((path-p Variable)) (path-equivalence path-p path-p)))
(assert (forall ((cs-a Bool) (path-p Variable) (path-q Variable) (path-r Variable) (path-s Variable) (var-x Variable)) (=> (and (=> cs-a (path-equivalence (substitute path-p var-x path-r) (substitute path-q var-x path-r))) (=> cs-a (path-equivalence path-s path-r))) (=> cs-a (path-equivalence (substitute path-p var-x path-s) (substitute path-q var-x path-s))))))
;(assert (not (forall ((p Variable) (q Variable)) (=> (path-equivalence q p) (path-equivalence p q)) )))  ; symmetry
(assert (not (forall ((a Variable) (b Variable) (c Variable))
              (=>
               (and (path-equivalence a b) (path-equivalence b c))
               (path-equivalence a c)) )))   ; transitivity
(check-sat)
