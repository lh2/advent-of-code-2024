(defpackage #:aoc/day-3
  (:use #:cl #:aoc/utils)
  (:export #:day-3))
(in-package #:aoc/day-3)

(defun try-read-number (stream)
  (loop while (digit-char-p (peek-char nil stream))
        collect (read-char stream) into bag
        finally (return (if bag
                            (parse-integer (coerce bag 'string))
                            nil))))

(define-parser day-3-parser (stream)
    ((mul-enabled t)
     (num-1 0)
     (num-2 0)
     (task-1 0)
     (task-2 0))
  (#\d #\o #\( #\) (setf mul-enabled t))
  (#\d #\o #\n #\' #\t #\( #\) (setf mul-enabled nil))
  (#\m #\u #\l #\(
       (setf num-1 (try-read-number stream))
       #\,
       (setf num-2 (try-read-number stream))
       #\)
       (let ((result (* num-1 num-2)))
         (incf task-1 result)
         (when mul-enabled
           (incf task-2 result)))))

(defun day-3 (input)
  (multiple-value-bind (mul-enabled num-1 num-2 task-1 task-2)
      (day-3-parser input)
    (declare (ignore mul-enabled num-1 num-2))
    (values task-1 task-2)))
