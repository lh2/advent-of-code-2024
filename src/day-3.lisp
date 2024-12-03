(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:import-from :cl-ppcre)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defparameter *regex* "(mul\\((\\d+),(\\d+)\\))|((do|don't)\\(\\))")

(defun day-3 (input)
  (let ((input-string (read-stream-content-into-string input))
        (mul-enabled t)
        (task-1 0)
        (task-2 0))
    (ppcre:do-scans (start end reg-starts reg-ends *regex* input-string)
      (case (aref input-string start)
        (#\d
         (setf mul-enabled (char/= (aref input-string (+ start 2)) #\n)))
        (#\m
         (let* ((num-1 (parse-integer input-string :start (aref reg-starts 1) :end (aref reg-ends 1)))
                (num-2 (parse-integer input-string :start (aref reg-starts 2) :end (aref reg-ends 2)))
                (result (* num-1 num-2)))
           (incf task-1 result)
           (when mul-enabled
             (incf task-2 result))))))
    (values task-1 task-2)))
