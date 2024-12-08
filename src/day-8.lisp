(defpackage #:aoc/day-8
  (:use #:cl #:aoc/utils)
  (:export #:day-8))
(in-package #:aoc/day-8)

(defun find-antennas (map)
  (loop with antennas = (make-hash-table)
        for x from 0 below (input-map-width map)
        do (loop for y from 0 below (input-map-height map)
                 for point = (cons x y)
                 for cell = (map-cell map point)
                 unless (char= cell #\.)
                   do (setf (gethash cell antennas)
                            (cons point (gethash cell antennas))))
        finally (return antennas)))

(defun antinode-location (location-1 location-2)
  (when (equal location-1 location-2)
    (return-from antinode-location nil))
  (point+ location-1 (point- location-1 location-2)))

(defun antinode-locations (location-1 location-2 width height)
  (when (equal location-1 location-2)
    (return-from antinode-locations nil))
  (loop with diff = (point- location-1 location-2)
        for point = (point+ (or point location-1) diff)
        while (point-in-bounds-p point width height)
        collect point))

(defun compute-antinodes (map antennas)
  (loop with antinodes-task-1 = (make-hash-table :test #'equal)
        with antinodes-task-2 = (make-hash-table :test #'equal)
        with map-width = (input-map-width map)
        with map-height = (input-map-height map)
        for antenna-locations being the hash-values of antennas
        do (loop for location-1 in antenna-locations
                 do (setf (gethash location-1 antinodes-task-2) t)
                 do (loop for location-2 in antenna-locations
                          for antinode-task-1 = (antinode-location location-1 location-2)
                          when (and antinode-task-1
                                    (point-in-bounds-p antinode-task-1 map-width map-height))
                            do (setf (gethash antinode-task-1 antinodes-task-1) t)
                          do (loop for antinode-task-2 in (antinode-locations location-1
                                                                              location-2
                                                                              map-width
                                                                              map-height)
                                   do (setf (gethash antinode-task-2 antinodes-task-2) t))))
        finally (return (values (hash-table-count antinodes-task-1)
                                (hash-table-count antinodes-task-2)))))

(defun day-8 (input)
  (let* ((map (make-map input))
         (antennas (find-antennas map)))
    (compute-antinodes map antennas)))
