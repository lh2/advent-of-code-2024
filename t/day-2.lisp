(defpackage #:aoc-test/day-2
  (:use #:cl #:lisp-unit2)
  (:import-from aoc/day-2))
(in-package #:aoc-test/day-2)

(define-test test-day-2
    ()
  ;;; Test resets
  ;; Report where the first item needs to be skipped
  (assert (aoc/day-2:report-safe-p (list 30 24 25 28 31 33 35) :allow-skip t))
  ;; Report where the second item needs to be skipped
  (assert (aoc/day-2:report-safe-p (list 24 21 25 28 31 33 35) :allow-skip t))
  ;; Report where reset target needs to be removed
  (assert (aoc/day-2:report-safe-p (list 25 22 19 21 20 17 14 13) :allow-skip t))
  ;;; Test task 1 & 2
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 2 "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")
    (assert= 2 task-1)
    (assert= 4 task-2)))
