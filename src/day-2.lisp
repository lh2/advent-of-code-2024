(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export #:day-2))
(in-package #:aoc/day-2)

(defun report-safe-p (report)
  (loop with dir = (if (> (first report) (second report)) #'> #'<)
        with last = (first report)
        for current in (cdr report)
        for diff = (abs (- last current))
        unless (funcall dir last current)
          do (return nil)
        unless (<= diff 3)
          do (return nil)
        do (setf last current)
        finally (return t)))

(defun remove-nth (n list)
  (concatenate 'list
               (subseq list 0 n)
               (subseq list (1+ n))))

(defun report-somewhat-safe-p (report)
  (or (report-safe-p report)
      (loop for i from 0 below (length report)
            for nr = (remove-nth i report)
            when (report-safe-p nr)
              do (return t))))

(defun day-2 (input)
  (loop for line = (read-line input nil)
        until (null line)
        for report = (read-number-list line)
        when (report-safe-p report)
          sum 1 into task-1
        when (report-somewhat-safe-p report)
          sum 1 into task-2
        finally (return (values task-1 task-2))))
