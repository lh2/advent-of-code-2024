(defpackage #:aoc-test/day-6
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-6))
(in-package #:aoc-test/day-6)

(define-test test-day-6
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 6 "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")
    (assert= 41 task-1)
    (assert= 6 task-2)))
