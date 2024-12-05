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

(defun day-5 (input)
  (loop with rule-map = (make-hash-table :test #'equal)
        with processing-updates = nil
        with task-1 = 0
        with task-2 = 0
        for line = (read-line input nil)
        until (null line)
        do (cond
             (processing-updates
              (let* ((update (parse-update line))
                     (sorted (order-update (copy-seq update) rule-map))
                     (middle (middle-page-number sorted)))
                (if (seq= update sorted)
                    (incf task-1 middle)
                    (incf task-2 middle))))
             ((string= line "")
              (setf processing-updates t))
             (t (setf (gethash (parse-rule line) rule-map) t)))
        finally (return (values task-1 task-2))))
