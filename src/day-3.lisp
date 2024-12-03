(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defun day-3 (input)
  (let ((input-string (read-stream-content-into-string input))
        (mul-enabled t)
        (task-1 0)
        (task-2 0))
    (ppcre:do-scans (start end reg-starts reg-ends "(mul\\(\\d+,\\d+\\))|((do|don't)\\(\\))" input-string)
      (case (aref input-string start)
        (#\d
         (setf mul-enabled (char/= (aref input-string (+ start 2)) #\n)))
        (#\m
         (multiple-value-bind (num-1 end)
             (parse-integer input-string :start (+ start 4) :junk-allowed t)
           (let* ((num-2 (parse-integer input-string :start (1+ end) :junk-allowed t))
                  (result (* num-1 num-2)))
             (incf task-1 result)
             (when mul-enabled
               (incf task-2 result)))))))
    (values task-1 task-2)))
