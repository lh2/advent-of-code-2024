(defpackage #:aoc/day-25
  (:use #:cl #:aoc/utils)
  (:export
   #:key-fits-p
   #:day-25))
(in-package #:aoc/day-25)

(defun heightmap (map)
  (loop for x from 0 below (input-map-width map)
        collect (loop for y from 0 below (input-map-height map)
                      when (char= (map-cell map (cons x y)) #\#)
                        sum 1)))

(defun parse-input (input)
  (loop with locks = nil
        with keys = nil
        with max-height = 0
        for map = (make-map input)
        until (null map)
        for height = (input-map-height map)
        when (> height max-height)
          do (setf max-height height)
        if (char= (map-cell map (cons 0 0)) #\.)
          do (push (heightmap map) keys)
        else
          do (push (heightmap map) locks)
        finally (return (values locks keys max-height))))

(defun key-fits-p (lock key max-height)
  (loop for lock-v in lock
        for key-v in key
        always (<= (+ lock-v key-v) max-height)))

(defun day-25 (input)
  (multiple-value-bind (locks keys max-height)
      (parse-input input)
    (loop for lock in locks
          sum (loop for key in keys
                    when (key-fits-p lock key max-height)
                      sum 1))))
