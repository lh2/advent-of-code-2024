(defpackage #:aoc/day-14
  (:use #:cl #:aoc/utils)
  (:export
   #:parse-robots
   #:task-1
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

(declaim (inline robot-after))

(defun robot-after (robot seconds bounds)
  (point-mod (point+ (first robot)
                     (point* (second robot) (cons seconds seconds)))
             bounds))

(defun task-1 (robots width height)
  (loop with bounds = (cons width height)
        with hmiddle = (floor width 2)
        with vmiddle = (floor height 2)
        with quadrants = (list 0 0 0 0)
        for robot in robots
        for new-pos = (robot-after robot 100 bounds)
        for x = (point-x new-pos)
        for y = (point-y new-pos)
        unless (or (= x hmiddle) (= y vmiddle))
          do (incf (nth (+ (* (round x width) 2)
                           (round y height))
                        quadrants))
        finally (return (apply #'* quadrants))))

(defun task-2 (robots width height)
  (loop with bounds = (cons width height)
        for seconds from 1
        for map = (make-hash-table :test #'equal)
        when (loop for robot in robots
                   for new-pos = (robot-after robot seconds bounds)
                   when (gethash new-pos map)
                     do (return nil)
                   do (setf (gethash new-pos map) t)
                   finally (return t))
          do (return seconds)))

(defun day-14 (input)
  (let ((robots (parse-robots input)))
    (values
     (task-1 robots 101 103)
     (task-2 robots 101 103))))
