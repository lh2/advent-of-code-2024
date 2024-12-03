(defpackage #:aoc/day-2
  (:use #:cl #:aoc/utils)
  (:export
   #:report-safe-p
   #:day-2))
(in-package #:aoc/day-2)

(defun report-safe-p (report &key direction allow-skip)
  (when (null direction)
    (return-from report-safe-p
      (or (report-safe-p report :direction #'< :allow-skip allow-skip)
          (report-safe-p report :direction #'> :allow-skip allow-skip))))
  (when (null (cdr report))
    (return-from report-safe-p t))
  (loop with last = nil
        for cdr on report
        until (null (cdr cdr))
        for left = (car cdr)
        for right = (cadr cdr)
        for diff = (abs (- left right))
        for cdir = (funcall direction left right)
        unless (and cdir (<= diff 3))
          do (return (when allow-skip
                       (or (report-safe-p (cons left (cddr cdr)) :direction direction)
                           (if (null last)
                               (report-safe-p (cdr cdr) :direction direction)
                               (report-safe-p (cons (car last) (cdr cdr)) :direction direction)))))
        do (setf last cdr)
        finally (return t)))

(defun day-2 (input)
  (loop for line = (read-line input nil)
        until (null line)
        for report = (read-number-list line)
        when (report-safe-p report)
          sum 1 into task-1
        when (report-safe-p report :allow-skip t)
          sum 1 into task-2
        finally (return (values task-1 task-2))))
