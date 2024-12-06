(defpackage #:aoc/day-6
  (:use #:cl #:aoc/utils)
  (:export #:day-6))
(in-package #:aoc/day-6)

(defun find-guard (map)
  (loop for y from 0 below (input-map-height map)
        thereis (loop for x from 0 below (input-map-width map)
                      for point = (cons x y)
                      for cell = (map-cell map point)
                      when (char= cell #\^)
                        do (return point))))

(defun next (point direction)
  (point+
   point
   (ecase direction
     (:up (cons 0 -1))
     (:left (cons -1 0))
     (:right (cons 1 0))
     (:down (cons 0 1)))))

(defun turn (direction)
  (ecase direction
    (:up :right)
    (:right :down)
    (:down :left)
    (:left :up)))

(defun walk-map (map pos)
  (loop with dir = :up
        with visited = (list pos)
        with visited-dir = (make-hash-table :test #'equal)
        with map-width = (input-map-width map)
        with map-height = (input-map-height map)
        with in-bounds-p = (lambda (point)
                             (and (>= (point-x point) 0)
                                  (>= (point-y point) 0)
                                  (< (point-x point) map-width)
                                  (< (point-y point) map-height)))
        while (funcall in-bounds-p pos)
        when (gethash (cons pos dir) visited-dir)
          do (return :loop)
        do (setf (gethash (cons pos dir) visited-dir) t
                 pos (loop with next = (next pos dir)
                           while (and (funcall in-bounds-p next)
                                      (char= (map-cell map next) #\#))
                           do (setf dir (turn dir)
                                    next (next pos dir))
                           finally (return next)))
           (push pos visited)
        finally (return visited)))

(defun task-2 (map initial-pos)
  (loop with task-2 = 0
        for y from 0 below (input-map-height map)
        do (loop for x from 0 below (input-map-width map)
                 for point = (cons x y)
                 for cell = (map-cell map point)
                 when (char= cell #\.)
                   do (setf (map-cell map point) #\#)
                      (when (eq (walk-map map initial-pos) :loop)
                        (incf task-2))
                      (setf (map-cell map point) #\.))
        finally (return task-2)))

(defun day-6 (input)
  (let* ((map (make-map input))
         (pos (find-guard map)))
    (values (1- (length (remove-duplicates (walk-map map pos) :test #'equal)))
            (task-2 map pos))))
