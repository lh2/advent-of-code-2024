(defpackage #:aoc/day-1
  (:use #:cl #:aoc/utils)
  (:export #:day-1))
(in-package #:aoc/day-1)

(defun day-1 (input)
  (let* ((numbers (mapcar #'read-number-list (read-input input)))
         (left (mapcar #'car numbers))
         (right (mapcar #'cadr numbers))
         (left (sort left #'<))
         (right (sort right #'<)))
    (values
     (loop for number-left in left
           for number-right in right
           for distance = (abs (- number-left number-right))
           sum distance)
     (loop for number-left in left
           for number-of-instances = (count number-left right)
           sum (* number-left number-of-instances)))))
