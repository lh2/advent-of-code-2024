(defpackage #:aoc/day-19
  (:use #:cl #:aoc/utils)
  (:export #:day-19))
(in-package #:aoc/day-19)

(defun parse-comma-list (line)
  (loop with pos = 0
        for comma-pos = (position #\, line :start pos)
        collect (subseq line pos comma-pos)
        do (setf pos (+ (or comma-pos 0) 2))
        until (null comma-pos)))

(defun parse-input (input)
  (loop with towels = (parse-comma-list (read-line input))
        initially (read-line input)
        for line = (read-line input nil)
        until (null line)
        collect line into lines
        finally (return (values towels lines))))

(defun possible-arrangements (line towels)
  (let ((cache (make-hash-table)))
    (labels ((%solve (i)
               (when (= i (length line))
                 (return-from %solve 1))
               (loop for towel in towels
                     for has-prefix = (string-prefix-p towel line :start2 i)
                     sum (or (and has-prefix (%solve-cached (+ i (length towel)))) 0)))
             (%solve-cached (i)
               (or (gethash i cache)
                   (setf (gethash i cache)
                         (%solve i)))))
      (%solve 0))))

(defun all-possible-arrangements (towels lines)
  (loop for line in lines
        collect (possible-arrangements line towels)))

(defun day-19 (input)
  (multiple-value-bind (towels lines)
      (parse-input input)
    (let ((a (all-possible-arrangements towels lines)))
      (values (count 0 a :test-not #'eq)
              (sum (remove nil a))))))
