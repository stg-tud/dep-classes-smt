; Old path using strings for variable and field names
; (declare-datatype Path ((var (id String)) (pth (obj Path) (field String))))
; (define-fun-rec subst-path ((p1 Path) (x String) (p2 Path)) Path (ite (is-var p1) (ite (= x (id p1)) p2 p1) (pth (subst-path (obj p1) x p2) (field p1))))


(declare-datatype Variable (X Y Z))
(declare-datatype Field (F G H))
(declare-datatype Path ((var (id Variable)) (pth (obj Path) (field Field))))

(define-fun-rec subst-path ((p1 Path) (x Variable) (p2 Path)) Path
  (ite (is-var p1)
       (ite (= x (id p1)) p2 p1)
       (pth (subst-path (obj p1) x p2) (field p1))))

(declare-const x Variable)
(declare-const p1 Path)
(declare-const p2 Path)
(declare-const p3 Path)

(assert (= x X))
(assert (= p1 (pth (var X) F)))      ; x.f
(assert (= p2 (var Y)))              ; y
(assert (= p3 (subst-path p1 x p2))) ; subst x in x.f with y
(assert (= p3 (pth (var Y) F)))      ; is y.f


; use result of subst to subst again
(declare-const y Variable)
(declare-const p4 Path)
(declare-const p5 Path)

(assert (= y Y))
(assert (= p4 (pth (var Z) G)))      ; z.g
(assert (= p5 (subst-path p3 y p4))) ; subst y in y.f with z.g
(assert (= p5 (pth (pth (var Z) G) F)))

(check-sat)
(get-model)
