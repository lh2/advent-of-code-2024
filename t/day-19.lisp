(defpackage #:aoc-test/day-19
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-19))
(in-package #:aoc-test/day-19)

(define-test test-day-19
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 19 "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb")
    (assert= 6 task-1)
    (assert= 16 task-2)))
