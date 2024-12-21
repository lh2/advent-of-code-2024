(defpackage #:aoc/day-21
  (:use #:cl #:aoc/utils)
  (:export #:day-21))
(in-package #:aoc/day-21)

(defparameter *keypad*
  (with-input-from-string (s "789
456
123
#0A")
    (make-map s)))

(defparameter *directional-pad*
  (with-input-from-string (s "#^A
<v>")
    (make-map s)))

(defparameter *directions* '((0 . -1) (1 . 0) (0 . 1) (-1 . 0)))

(defun direction (dir)
  (eswitch (dir :test #'equal)
    ('(1 . 0) #\>)
    ('(-1 . 0) #\<)
    ('(0 . 1) #\v)
    ('(0 . -1) #\^)))

(defun dfs (map start end width height max)
  (loop with stack = (list (list start nil 0))
        with possible-paths = nil
        for (pos dirs length) = (pop stack)
        when (null pos)
          do (return possible-paths)
        when (equal pos end)
          do (push (reverse dirs) possible-paths)
        when (< length max)
          do (loop for dir in *directions*
                   for next = (point+ pos dir)
                   when (and (point-in-bounds-p next width height)
                             (char/= (map-cell map next) #\#))
                     do (push (list next (cons (direction dir) dirs) (1+ length)) stack))))

(defun build-arm-movements-cache (map)
  (loop with cache = (make-hash-table :test #'equal)
        with width = (input-map-width map)
        with height = (input-map-height map)
        for y-1 from 0 below height
        do (loop for x-1 from 0 below width
                 for p-1 = (cons x-1 y-1)
                 for c-1 = (map-cell map p-1)
                 unless (char= c-1 #\#)
                   do (loop for y-2 from 0 below height
                            do (loop for x-2 from 0 below width
                                     for p-2 = (cons x-2 y-2)
                                     for c-2 = (map-cell map p-2)
                                     for max-distance = (manhattan-distance p-1 p-2)
                                     unless (or (char= c-2 #\#)
                                                (char= c-1 c-2))
                                       do (setf (gethash (cons c-1 c-2) cache)
                                                (dfs map p-1 p-2 width height max-distance)))))
        finally (return cache)))

(defparameter *keypad-movements* (build-arm-movements-cache *keypad*))
(defparameter *directional-pad-movements* (build-arm-movements-cache *directional-pad*))

(defun all-possibilities (buttons cache)
  (loop with all-possibilities = nil
        with last = #\A
        for button in buttons
        for possibilities = (gethash (cons last button) cache)
        if possibilities
          do (push (mapcar (rcurry #'append '(#\A)) possibilities) all-possibilities)
        else
          do (push '((#\A)) all-possibilities)
        do (setf last button)
        finally (return (nreverse all-possibilities))))

(defun make-robot (next movements-map)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (sequence)
      (loop for possibilities in (all-possibilities sequence movements-map)
            sum (loop for possibility in possibilities
                      minimize (or (gethash possibility cache)
                                   (setf (gethash possibility cache)
                                         (funcall next possibility))))))))

(defun make-robots (n-robots)
  (loop with current = (make-robot #'length *directional-pad-movements*)
        for i from 1 to n-robots
        do (setf current (make-robot current (if (= i n-robots)
                                                 *keypad-movements*
                                                 *directional-pad-movements*)))
        finally (return current)))

(defun day-21 (input)
  (loop with task-1-robots = (make-robots 2)
        with task-2-robots = (make-robots 25)
        for line = (read-line input nil)
        until (null line)
        for buttons = (coerce line 'list)
        for numeric-value = (parse-integer line :junk-allowed t)
        sum (* (funcall task-1-robots buttons) numeric-value) into task-1
        sum (* (funcall task-2-robots buttons) numeric-value) into task-2
        finally (return (values task-1 task-2))))
