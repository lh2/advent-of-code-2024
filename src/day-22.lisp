(defpackage #:aoc/day-22
  (:use #:cl #:aoc/utils)
  (:export #:day-22))
(in-package #:aoc/day-22)

(defun calculate-secret-number (secret n sequences)
  (loop with local-sequences = (make-hash-table :test #'equal)
        with last-price = 0
        with history = nil
        with price = 0
        repeat n
        for i from 1
        do (setf secret (mod (logxor secret (* secret 64)) 16777216)
                 secret (mod (logxor secret (floor secret 32)) 16777216)
                 secret (mod (logxor secret (* secret 2048)) 16777216)
                 price (mod secret 10))
           (when (> i 1)
             (push (- price last-price) history))
        when (> i 4)
          do (unless (gethash history local-sequences)
               (incf (gethash history sequences 0) price)
               (setf (gethash history local-sequences) t))
             (setf history (take 3 history))
        do (setf last-price price)
        finally (return secret)))

(defun best-sequence (sequences)
  (loop for bananas being the hash-values of sequences
        maximize bananas))

(defun day-22 (input)
  (loop with sequences = (make-hash-table :test #'equal)
        for line = (read-line input nil)
        until (null line)
        for initial-secret = (parse-integer line)
        sum (calculate-secret-number initial-secret 2000 sequences) into task-1
        finally (return (values task-1 (best-sequence sequences)))))
