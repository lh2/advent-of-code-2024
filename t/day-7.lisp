(defpackage #:aoc-test/day-7
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-7))
(in-package #:aoc-test/day-7)

(define-test test-day-7
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 7 "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")
    (assert= 3749 task-1)
    (assert= 11387 task-2)))
