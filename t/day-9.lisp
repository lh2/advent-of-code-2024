(defpackage #:aoc-test/day-9
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-9))
(in-package #:aoc-test/day-9)

(define-test test-day-9
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 9 "2333133121414131402")
    (assert= 1928 task-1)
    (assert= 2858 task-2)))
