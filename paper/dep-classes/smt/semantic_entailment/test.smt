(declare-const i Int)
(assert (forall ((j Int)) (not (= i j))))
(check-sat)
