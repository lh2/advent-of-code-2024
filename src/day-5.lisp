(defpackage #:aoc/day-5
  (:use #:cl #:aoc/utils)
  (:export #:day-5))
(in-package #:aoc/day-5)

(defun parse-rule (line)
  (mapcar #'parse-integer
          (uiop:split-string line :separator '(#\|))))

(defun parse-update (line)
  (map 'vector
       #'parse-integer
       (uiop:split-string line :separator '(#\,))))

(defun update-ordered-p (update rules)
  (loop for (first second) in rules
        for pos-first = (position first update)
        for pos-second = (position second update)
        unless (or
                (null pos-first)
                (null pos-second)
                (< pos-first pos-second))
          do (return nil)
        finally (return t)))

(defun order-update (update rule-map)
  (sort update (lambda (first second)
                 (not (gethash (list second first) rule-map)))))

(defun middle-page-number (update)
  (aref update (floor (length update) 2)))

(defun day-5 (input)
  (loop with rules = nil
        with rule-map = (make-hash-table :test #'equal)
        with processing-updates = nil
        with task-1 = 0
        with task-2 = 0
        for line = (read-line input nil)
        until (null line)
        do (cond
             (processing-updates
              (let ((update (parse-update line)))
                (if (update-ordered-p update rules)
                    (incf task-1 (middle-page-number update))
                    (progn
                      (setf update (order-update update rule-map))
                      (incf task-2 (middle-page-number update))))))
             ((string= line "")
              (setf processing-updates t))
             (t (let ((rule (parse-rule line)))
                  (setf (gethash rule rule-map) t)
                  (push rule rules))))
        finally (return (values task-1 task-2))))
