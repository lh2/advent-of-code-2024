(defpackage #:aoc/day-7
  (:use #:cl #:aoc/utils)
  (:export #:day-7))
(in-package #:aoc/day-7)

(defparameter *operators-task-1* '(+ *))
(defparameter *operators-task-2* '(+ * combine))

(defun map-operation-combinations (function length operators)
  (apply #'map-product function (loop for n from 1 below length
                                      collect operators)))

(defun number-digits (number)
  (if (zerop number)
      1
      (1+ (floor (log number 10)))))

(defun combine (num-1 num-2)
  (+ (* num-1 (expt 10 (number-digits num-2))) num-2))

(defun calculate (operators numbers)
  (loop with current = (car numbers)
        for operator in operators
        for next in (cdr numbers)
        do (setf current (funcall operator current next))
        finally (return current)))

(defun test-value-valid-p (test-value numbers operators)
  (map-operation-combinations (lambda (&rest operators)
                                (when (= test-value (calculate operators numbers))
                                  (return-from test-value-valid-p t)))
                              (length numbers)
                              operators)
  nil)

(defun day-7 (input)
  (loop for line = (read-line input nil)
        until (null line)
        for (test-value . numbers) = (read-number-list line)
        when (test-value-valid-p test-value numbers *operators-task-1*)
          sum test-value into task-1
        when (test-value-valid-p test-value numbers *operators-task-2*)
          sum test-value into task-2
        finally (return (values task-1 task-2))))
