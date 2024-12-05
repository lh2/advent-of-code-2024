(defpackage #:aoc-test/day-5
  (:use #:cl #:lisp-unit2)
  (:import-from #:aoc/day-5))
(in-package #:aoc-test/day-5)

(define-test test-day-5
    ()
  (multiple-value-bind (task-1 task-2)
      (aoc:run-day 5 "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")
    (assert= 143 task-1)
    (assert= 123 task-2)))
