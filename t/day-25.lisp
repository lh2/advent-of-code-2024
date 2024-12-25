(defpackage #:aoc-test/day-25
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-25))
(in-package #:aoc-test/day-25)

(define-test test-day-25
    ()
  (assert-eq nil (aoc/day-25:key-fits-p '(0 5 3 4 3) '(5 0 2 1 3) 5))
  (assert-eq nil (aoc/day-25:key-fits-p '(0 5 3 4 3) '(4 3 4 0 2) 5))
  (assert-eq t   (aoc/day-25:key-fits-p '(0 5 3 4 3) '(3 0 2 0 1) 5))
  (assert-eq nil (aoc/day-25:key-fits-p '(1 2 0 5 3) '(5 0 2 1 3) 5))
  (assert-eq t   (aoc/day-25:key-fits-p '(1 2 0 5 3) '(4 3 4 0 2) 5))
  (assert-eq t   (aoc/day-25:key-fits-p '(1 2 0 5 3) '(3 0 2 0 1) 5))
  (assert= 3 (aoc:run-day 25 "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####")))
