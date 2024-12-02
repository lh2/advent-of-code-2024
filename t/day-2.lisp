(defpackage #:aoc-test/day-2
  (:use #:cl #:lisp-unit2))
(in-package #:aoc-test/day-2)

(define-test test-day-2
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 2 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
    (assert= 2 task-1)
    (assert= 4 task-2)))
