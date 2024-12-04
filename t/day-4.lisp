(defpackage #:aoc-test/day-4
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-4))
(in-package #:aoc-test/day-4)

(define-test test-day-4
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 4 "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")
    (assert= 18 task-1)
    (assert= 9 task-2)))
