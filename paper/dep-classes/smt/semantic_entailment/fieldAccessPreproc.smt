(declare-datatype Class ((Zero) (Nat) (Succ)))
(declare-datatype Variable ((x)))
(declare-datatype Field ((p)))
(declare-datatype Path ((var (id Variable)) (pth (obj Path) (field Field))))
(declare-fun instance-of (Path Class) Bool)
(declare-fun instantiated-by (Path Class) Bool)
(declare-fun path-equivalence (Path Path) Bool)
(define-fun-rec substitute ((path-p Path) (var-x Variable) (path-q Path)) Path (ite (is-var path-p) (ite (= var-x (id path-p)) path-q path-p) (pth (substitute (obj path-p) var-x path-q) (field path-p))))
(assert (forall ((path-p Path)) (path-equivalence path-p path-p)))
(assert (forall ((cs-a Bool) (path-p Path) (class-c Class)) (=> (=> cs-a (instantiated-by path-p class-c)) (=> cs-a (instance-of path-p class-c)))))
(assert (forall ((cs-a Bool) (path-p Path) (path-q Path) (path-r Path) (path-s Path) (var-x Variable)) (=> (and (=> cs-a (path-equivalence (substitute path-p var-x path-r) (substitute path-q var-x path-r))) (=> cs-a (path-equivalence path-s path-r))) (=> cs-a (path-equivalence (substitute path-p var-x path-s) (substitute path-q var-x path-s))))))
(assert (forall ((cs-a Bool) (path-p Path) (class-c Class) (path-r Path) (path-s Path) (var-x Variable)) (=> (and (=> cs-a (instance-of (substitute path-p var-x path-r) class-c)) (=> cs-a (path-equivalence path-s path-r))) (=> cs-a (instance-of (substitute path-p var-x path-s) class-c)))))
(assert (forall ((cs-a Bool) (path-p Path) (class-c Class) (path-r Path) (path-s Path) (var-x Variable)) (=> (and (=> cs-a (instantiated-by (substitute path-p var-x path-r) class-c)) (=> cs-a (path-equivalence path-s path-r))) (=> cs-a (instantiated-by (substitute path-p var-x path-s) class-c)))))
(assert (forall ((cs-a Bool) (path-p Path)) (=> (=> cs-a (instance-of path-p Zero)) (=> cs-a (instance-of path-p Nat)))))
(assert (forall ((cs-a Bool) (path-p Path)) (=> (=> cs-a (and (instance-of path-p Succ) (instance-of (pth path-p p) Nat))) (=> cs-a (instance-of path-p Nat)))))
(assert (not (=> (and (instance-of (var x) Succ) (instance-of (pth (var x) p) Zero)) (instance-of (var x) Zero))))
(check-sat)
