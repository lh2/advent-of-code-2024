(defpackage #:aoc-test/day-18
  (:use #:cl #:lisp-unit2 #:aoc/day-18))
(in-package #:aoc-test/day-18)

(define-test test-day-18
    ()
  (let ((*width* 7)
        (*height* 7)
        (*bytes-falling* 12))
    (multiple-value-bind (task-1 task-2)
        (aoc:run-day 18 "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0")
      (assert= 22 task-1)
      (assert-string-equal "6,1" task-2))))
