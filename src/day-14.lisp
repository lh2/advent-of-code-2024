(defpackage #:aoc/day-14
  (:use #:cl #:aoc/utils)
  (:export
   #:parse-robots
   #:run
   #:day-14))
(in-package #:aoc/day-14)

(defun parse-robots (input)
  (loop for line = (read-line input nil)
        until (null line)
        collect (parse-robot line)))

(defun parse-robot (line)
  (let* ((pspace (position #\Space line))
         (p (uiop:split-string (subseq line 2 pspace) :separator '(#\,)))
         (v (uiop:split-string (subseq line (+ pspace 3)) :separator '(#\,)))
         (p (mapcar #'parse-integer p))
         (v (mapcar #'parse-integer v)))
    (list (cons (first p) (second p))
          (cons (first v) (second v)))))

(defun safety-factor (robots width height)
  (loop with hmiddle = (floor width 2)
        with vmiddle = (floor height 2)
        with ul = 0
        with ur = 0
        with bl = 0
        with br = 0
        for robot in robots
        for pos = (first robot)
        for x = (point-x pos)
        for y = (point-y pos)
        do (cond
             ((and (< x hmiddle) (< y vmiddle))
              (incf ul))
             ((and (> x hmiddle) (< y vmiddle))
              (incf ur))
             ((and (< x hmiddle) (> y vmiddle))
              (incf bl))
             ((and (> x hmiddle) (> y vmiddle))
              (incf br)))
        finally (return (* ul ur bl br))))

(defun run (robots width height &optional with-task-2)
  (loop with task-1 = nil
        for i from 1
        for overlap-set = (make-hash-table :test #'equal)
        for overlaps? = nil
        do (loop for robot in robots
                 for pos = (first robot)
                 for velocity = (second robot)
                 for new-pos = (point+ pos velocity)
                 do (setf (point-x new-pos)
                          (mod (point-x new-pos) width)
                          (point-y new-pos)
                          (mod (point-y new-pos) height)
                          (first robot) new-pos)
                    (unless overlaps?
                      (setf overlaps? (gethash new-pos overlap-set)
                            (gethash new-pos overlap-set) t)))
        unless overlaps?
          do (return (values task-1 i))
        when (= i 100)
          do (setf task-1 (safety-factor robots width height))
          and unless with-task-2
                do (return task-1)))

(defun day-14 (input)
  (run (parse-robots input) 101 103 t))
