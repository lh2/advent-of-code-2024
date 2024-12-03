(defpackage #:aoc-test/day-1
  (:use #:cl #:lisp-unit2)
  (:import-from aoc/day-1))
(in-package #:aoc-test/day-1)

(define-test test-day-1
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 1 "3   4
4   3
2   5
1   3
3   9
3   3")
    (assert= 11 task-1)
    (assert= 31 task-2)))
