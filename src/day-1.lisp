(defpackage #:aoc/day-1
  (:use #:cl #:aoc/utils)
  (:export #:day-1))
(in-package #:aoc/day-1)

(defun day-1 (input)
  (let ((right-counts (make-hash-table))
        left
        right)
    (loop for line = (read-line input nil)
          until (null line)
          for row = (read-number-list line)
          for right-num = (cadr row)
          do (push (car row) left)
             (push right-num right)
             (ensure-gethash right-num right-counts 0)
             (incf (gethash right-num right-counts))
          finally (setf left (sort left #'<)
                        right (sort right #'<)))
    (values
     (loop for number-left in left
           for number-right in right
           for distance = (abs (- number-left number-right))
           sum distance)
     (loop for number-left in left
           for number-of-instances = (or (gethash number-left right-counts) 0)
           sum (* number-left number-of-instances)))))
