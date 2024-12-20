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

(defun bfs (map start end)
  (loop with width = (input-map-width map)
        with height = (input-map-height map)
        with queue = (make-queue :simple-queue)
        with visited = (make-hash-table :test #'equal)
        initially (qpush queue (list start (list start)))
                  (setf (gethash start visited) t)
        for (pos steps) = (qpop queue)
        when (null pos)
          do (return nil)
        when (equal pos end)
          do (return (nreverse steps))
        do (loop for dir in *directions*
                 for next = (point+ pos dir)
                 when (and (point-in-bounds-p next width height)
                           (not (gethash next visited))
                           (char/= (map-cell map next) #\#))
                   do (qpush queue (list next (cons next steps)))
                      (setf (gethash next visited) t))))

(defun count-cheats (steps threshold)
  (let ((cache (make-hash-table :test #'equal)))
    (loop for remaining-picoseconds downfrom (1- (length steps))
          for step in steps
          do (setf (gethash step cache) remaining-picoseconds))
    (loop with task-1 = 0
          with task-2 = 0
          for node on steps
          for step-1 = (car node)
          for rps-1 = (gethash step-1 cache)
          do (loop for step-2 in (cdr node)
                   for dist = (manhattan-distance step-1 step-2)
                   for rps-2 = (gethash step-2 cache)
                   for saved = (- rps-1 (+ rps-2 dist))
                   do (when (>= saved threshold)
                        (when (<= dist 2)
                          (incf task-1))
                        (when (<= dist 20)
                          (incf task-2))))
          finally (return (values task-1 task-2)))))

(defun day-20 (input)
  (let ((map (make-map input)))
    (multiple-value-bind (start end)
        (find-start-end map)
      (count-cheats (bfs map start end) 100))))
