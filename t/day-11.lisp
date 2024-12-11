(defpackage #:aoc-test/day-11
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-11))
(in-package #:aoc-test/day-11)

(define-test test-day-11
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 11 "125 17")
    (assert= 55312 task-1)
    (assert= 65601038650482 task-2)))
