(defpackage #:aoc-test/day-3
  (:use #:cl #:lisp-unit2)
  (:import-from aoc/day-3))
(in-package #:aoc-test/day-3)

(define-test test-day-3
    ()
  (assert= 161 (aoc:run-day 3 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"))
  (assert= 48 (nth-value 1 (aoc:run-day 3 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"))))
