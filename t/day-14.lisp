(defpackage #:aoc-test/day-14
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-14))
(in-package #:aoc-test/day-14)

(define-test test-day-14
    ()
  (with-input-from-string (s "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")
    (assert= 12 (aoc/day-14:task-1 (aoc/day-14:parse-robots s) 11 7))))
