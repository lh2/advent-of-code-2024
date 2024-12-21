(defpackage #:aoc-test/day-21
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-21))
(in-package #:aoc-test/day-21)

(define-test test-day-21
    ()
  (assert= 126384 (aoc:run-day 21 "029A
980A
179A
456A
379A")))
