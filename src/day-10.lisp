(defpackage #:aoc/day-10
  (:use #:cl #:aoc/utils)
  (:export #:day-10))
(in-package #:aoc/day-10)

(defun find-trailheads (map)
  (loop for y from 0 below (input-map-height map)
        nconc (loop for x from 0 below (input-map-width map)
                    for point = (cons x y)
                    for cell = (map-cell map point)
                    when (char= cell #\0)
                      collect point)))

(defparameter *trail-neighbours*
  (list (cons 0 -1)
        (cons -1 0)
        (cons 1 0)
        (cons 0 1)))

(defun score (map trailhead)
  (let ((map-width (input-map-width map))
        (map-height (input-map-height map))
        (points (make-hash-table :test #'equal))
        (rating 0))
    (labels ((%walk (pos)
               (when (char= (map-cell map pos) #\9)
                 (setf (gethash pos points) t)
                 (incf rating)
                 (return-from %walk nil))
               (loop with height = (char-number (map-cell map pos))
                     for np in (loop for nd in *trail-neighbours*
                                     for np = (point+ pos nd)
                                     when (point-in-bounds-p np map-width map-height)
                                       collect np)
                     for nc = (map-cell map np)
                     when (and (not (char= nc #\.))
                               (= (- (char-number nc)
                                     height)
                                  1))
                       do (%walk np))))
      (%walk trailhead)
      (values (length (hash-table-keys points))
              rating))))

(defun day-10 (input)
  (loop with map = (make-map input)
        with trailheads = (find-trailheads map)
        for trailhead in trailheads
        for (score rating) = (multiple-value-list (score map trailhead))
        sum score into task-1
        sum rating into task-2
        finally (return (values task-1 task-2))))
