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

(declaim (ftype (function (fixnum fixnum) (simple-array t (* * 4))) make-visited-cache))
(defun make-visited-cache (width height)
  (make-array (list width height 4)
              :initial-element nil))

(declaim (inline visited-p
                 (setf visited-p)))

(defun visited-p (cache point dir)
  (destructuring-bind (x . y) point
    (aref cache x y (ecase dir
                      (:up 0)
                      (:left 1)
                      (:right 2)
                      (:down 3)))))

(defun (setf visited-p) (new-value cache point dir)
  (destructuring-bind (x . y) point
    (setf (aref cache x y (ecase dir
                            (:up 0)
                            (:left 1)
                            (:right 2)
                            (:down 3)))
          new-value)))

(defun walk-map (map pos &optional check-loops)
  (loop with dir = :up
        with map-width = (input-map-width map)
        with map-height = (input-map-height map)
        with visited-dir = (make-visited-cache map-width map-height)
        with visited = (make-array (list map-width map-height)
                                   :initial-element nil)
        with count-visited fixnum = 0
        while (point-in-bounds-p pos map-width map-height)
        when (and check-loops (visited-p visited-dir pos dir))
          do (return :loop)
        when check-loops
          do (setf (visited-p visited-dir pos dir) t)
        do (destructuring-bind (x . y) pos
             (unless (aref visited x y)
               (incf count-visited)
               (setf (aref visited x y) t)))
           (setf pos (loop with next = (next pos dir)
                           while (and (point-in-bounds-p next map-width map-height)
                                      (char= (map-cell map next) #\#))
                           do (setf dir (turn dir)
                                    next (next pos dir))
                           finally (return next)))
        finally (return (values count-visited visited))))

(defun task-2 (map initial-pos visited)
  (loop with task-2 = 0
        for x from 0 below (input-map-width map)
        do (loop for y from 0 below (input-map-height map)
                 for point = (cons x y)
                 when (and (aref visited x y)
                           (char= (map-cell map point) #\.))
                   do (setf (map-cell map point) #\#)
                      (when (eq (walk-map map initial-pos t) :loop)
                        (incf task-2))
                      (setf (map-cell map point) #\.))
        finally (return task-2)))

(defun day-6 (input)
  (let* ((map (make-map input))
         (pos (find-guard map)))
    (multiple-value-bind (task-1 visited)
        (walk-map map pos)
      (values task-1 (task-2 map pos visited)))))
