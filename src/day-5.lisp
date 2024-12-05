(defpackage #:aoc/day-5
  (:use #:cl #:aoc/utils)
  (:export #:day-5))
(in-package #:aoc/day-5)

(defun parse-rule (line)
  (mapcar #'parse-integer
          (uiop:split-string line :separator '(#\|))))

(defun parse-update (line)
  (mapcar #'parse-integer
          (uiop:split-string line :separator '(#\,))))

(defun order-update (update rule-map)
  (sort update (lambda (first second)
                 (not (gethash (list second first) rule-map)))))

(defun middle-page-number (update)
  (nth (floor (length update) 2) update))

(defun process-updates (input rule-map)
  (loop for line = (read-line input nil)
        until (null line)
        for update = (parse-update line)
        for sorted = (order-update (copy-seq update) rule-map)
        for middle = (middle-page-number sorted)
        if (seq= update sorted)
          sum middle into task-1
        else
          sum middle into task-2
        finally (return (values task-1 task-2))))

(defun day-5 (input)
  (loop with rule-map = (make-hash-table :test #'equal)
        with task-1 = 0
        with task-2 = 0
        for line = (read-line input nil)
        when (string= line "")
          do (return (process-updates input rule-map))
        do (setf (gethash (parse-rule line) rule-map) t)))
