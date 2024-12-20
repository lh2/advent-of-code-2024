(defpackage #:aoc/day-20
  (:use #:cl #:aoc/utils)
  (:export #:day-20))
(in-package #:aoc/day-20)

(defparameter *directions* '((1 . 0) (-1 . 0)
                             (0 . 1) (0 . -1)))

(defun find-start-end (map)
  (loop with start = nil
        with end = nil
        for y from 0 below (input-map-height map)
        thereis (loop for x from 0 below (input-map-width map)
                      for pos = (cons x y)
                      for cell = (map-cell map pos)
                      when (char= #\S cell)
                        do (setf start pos)
                      when (char= #\E cell)
                        do (setf end pos))
        finally (return (values start end))))

(defun day-20 (input)
  (loop with map = (make-map input)
        with (start end) = (multiple-value-list (find-start-end map))
        with width = (input-map-width map)
        with height = (input-map-height map)
        with pos = start
        with last = nil
        with steps = nil
        with task-1 = 0
        with task-2 = 0
        for picoseconds from 0
        do (loop for (pos-2 . picoseconds-2) in steps
                 for distance = (manhattan-distance pos pos-2)
                 for saved = (- picoseconds (+ picoseconds-2 distance))
                 do (when (>= saved 100)
                      (when (<= distance 2)
                        (incf task-1))
                      (when (<= distance 20)
                        (incf task-2))))
           (push (cons pos picoseconds) steps)
        when (equal pos end)
          do (return (values task-1 task-2))
        do (psetf last pos
                  pos (loop for dir in *directions*
                            for next = (point+ pos dir)
                            when (and (point-in-bounds-p next width height)
                                      (not (equal last next))
                                      (char/= (map-cell map next) #\#))
                              do (return next)))))
