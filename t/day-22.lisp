(defpackage #:aoc-test/day-22
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-22))
(in-package #:aoc-test/day-22)

(define-test test-day-22
    ()
  (assert= 37327623 (aoc:run-day 22 "1
10
100
2024"))
  (assert= 23 (nth-value 1 (aoc:run-day 22 "1
2
3
2024"))))
