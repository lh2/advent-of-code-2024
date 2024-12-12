(defpackage #:aoc-test/day-12
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-12))
(in-package #:aoc-test/day-12)

(define-test test-day-12
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 12 "AAAA
BBCD
BBCC
EEEC")
    (assert= 140 task-1)
    (assert= 80 task-2))
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 12 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")
    (assert= 772 task-1)
    (assert= 436 task-2))
  (assert= 1930 (aoc:run-day 12 "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE"))
  (assert= 236 (nth-value 1 (aoc:run-day 12 "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")))
  (assert= 368 (nth-value 1 (aoc:run-day 12 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA"))))
