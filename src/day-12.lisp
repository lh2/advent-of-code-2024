(defpackage #:aoc/day-12
  (:use #:cl #:aoc/utils)
  (:export #:day-12))
(in-package #:aoc/day-12)

(defparameter *neighbour-diffs* '((0 . -1) (-1 . 0) (1 . 0) (0 . 1)))

(defun walk-line (pos-1 pos-2 positions visited diff)
  (loop for current-1 = (point+ (or current-1 pos-1) diff)
        for current-2 = (point+ (or current-2 pos-2) diff)
        for set = (list current-1 current-2)
        while (and (gethash set positions)
                   (not (gethash set visited)))
        do (setf (gethash set visited) t)))

(defun find-straight-fences (positions)
  (loop with visited = (make-hash-table :test #'equal)
        with fences = 0
        for set being the hash-key of positions
        for diffs = (if (= (point-x (point- (first set) (second set))) 0)
                        '((-1 . 0) (1 . 0))
                        '((0 . -1) (0 . 1)))
        do (unless (gethash set visited)
             (loop for diff in diffs
                   do (walk-line (first set) (second set) positions visited diff))
             (incf fences)
             (setf (gethash set visited) t))
        finally (return fences)))

(defun region-properties (map point visited width height)
  (setf (gethash point visited) t)
  (loop with plant = (map-cell map point)
        with fences = 0
        with fence-positions = (make-hash-table :test #'equal)
        with todo = (list point)
        with area = 0
        while todo
        for current = (pop todo)
        do (incf area)
           (loop for neighbour-diff in *neighbour-diffs*
                 for neighbour = (point+ current neighbour-diff)
                 for in-bounds = (point-in-bounds-p neighbour width height)
                 for neighbour-plant = (and in-bounds (map-cell map neighbour))
                 do (if (and in-bounds (char= plant neighbour-plant))
                        (unless (gethash neighbour visited)
                          (push neighbour todo)
                          (setf (gethash neighbour visited) t))
                        (progn
                          (incf fences)
                          (setf (gethash (list current neighbour) fence-positions) t))))
        finally (return (values area fences (find-straight-fences fence-positions)))))

(defun day-12 (input)
  (loop with map = (make-map input)
        with visited = (make-hash-table :test #'equal)
        with cost-1 = 0
        with cost-2 = 0
        with width = (input-map-width map)
        with height = (input-map-height map)
        for y from 0 below height
        do (loop for x from 0 below width
                 for point = (cons x y)
                 unless (gethash point visited)
                   do (multiple-value-bind (area fences straight-fences)
                          (region-properties map point visited width height)
                        (incf cost-1 (* area fences))
                        (incf cost-2 (* area straight-fences))))
        finally (return (values cost-1 cost-2))))
