(defpackage #:aoc/day-7
  (:use #:cl #:aoc/utils)
  (:export #:day-7))
(in-package #:aoc/day-7)

(declaim (inline number-digits combine)
         (ftype (function (fixnum) fixnum) number-digits)
         (ftype (function (fixnum fixnum) fixnum) combine))

(defun number-digits (number)
  (if (zerop number)
      1
      (1+ (floor (log number 10)))))

(defun combine (num-1 num-2)
  (+ (the fixnum (* num-1
                    (the fixnum (expt 10 (number-digits num-2)))))
     num-2))

(defun test-value-valid-p (test-value numbers &optional combine-op)
  (labels ((%solve (test-value numbers combine-op current)
             (declare (type fixnum current test-value))
             (if (null numbers)
                 (= current test-value)
                 (if (> current test-value)
                     nil
                     (or (%solve test-value (cdr numbers) combine-op
                                 (+ current (the fixnum (car numbers))))
                         (%solve test-value (cdr numbers) combine-op
                                 (* current (the fixnum (car numbers))))
                         (and combine-op
                              (%solve test-value (cdr numbers) combine-op
                                      (combine current (car numbers)))))))))
    (%solve test-value (cdr numbers) combine-op (car numbers))))

(defun day-7 (input)
  (loop for line = (read-line input nil)
        until (null line)
        for (test-value . numbers) = (read-number-list line)
        when (test-value-valid-p test-value numbers)
          sum test-value into task-1
        when (test-value-valid-p test-value numbers t)
          sum test-value into task-2
        finally (return (values task-1 task-2))))
