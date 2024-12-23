(defpackage #:aoc-test/day-23
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-23))
(in-package #:aoc-test/day-23)

(define-test test-day-23
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 23 "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn")
    (assert= 7 task-1)
    (assert-string= "co,de,ka,ta" task-2)))
