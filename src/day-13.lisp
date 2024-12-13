(defpackage #:aoc/day-13
  (:use #:cl #:aoc/utils)
  (:export #:day-13))
(in-package #:aoc/day-13)

(defstruct claw-machine
  (button-a)
  (button-b)
  (prize))

(defun parse-coord-line (line)
  (let* ((offset (if (char= (aref line 0) #\P) 1 0))
         (colon (position #\: line))
         (pair (uiop:split-string (subseq line (+ colon 2)) :separator '(#\,)))
         (x (first pair))
         (y (second pair))
         (x (parse-integer x :start (1+ offset)))
         (y (parse-integer y :start (+ 2 offset))))
    (cons x y)))

(defun parse-claw-machine (input)
  (let ((l-1 (read-line input nil))
        (l-2 (read-line input nil))
        (l-3 (read-line input nil)))
    (read-line input nil)
    (when (or (null l-1) (null l-2) (null l-3))
      (return-from parse-claw-machine nil))
    (make-claw-machine :button-a (parse-coord-line l-1)
                       :button-b (parse-coord-line l-2)
                       :prize (parse-coord-line l-3))))

(defun parse-claw-machines (input)
  (loop for machine = (parse-claw-machine input)
        until (null machine)
        collect machine))

(defun play (machine prize-x prize-y)
  (let* ((button-a (claw-machine-button-a machine))
         (button-b (claw-machine-button-b machine))
         (xb (* (point-x button-b) (point-y button-a)))
         (yb (* (point-y button-b) (point-x button-a)))
         (px (* prize-x (point-y button-a)))
         (py (* prize-y (point-x button-a)))
         (b (/ (- py px) (- yb xb)))
         (a (/ (- prize-x (* (point-x button-b) b))
               (point-x button-a))))
    (when (and (integerp a)
               (integerp b))
      (+ (* a 3) b))))

(defun day-13 (input)
  (loop with machines = (parse-claw-machines input)
        for machine in machines
        for prize = (claw-machine-prize machine)
        for prize-x = (point-x prize)
        for prize-y = (point-y prize)
        sum (or (play machine prize-x prize-y) 0) into task-1
        sum (or (play machine
                      (+ prize-x 10000000000000)
                      (+ prize-y 10000000000000))
                0) into task-2
        finally (return (values task-1 task-2))))
