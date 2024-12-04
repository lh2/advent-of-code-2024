(defpackage #:aoc/day-4
  (:use #:cl #:aoc/utils)
  (:export #:day-4))
(in-package #:aoc/day-4)

(defun count-xmas (map point)
  (loop for (nx . ny) in *map-neighbours*
        sum (loop for (m char) in '((1 #\M) (2 #\A) (3 #\S))
                  for new-point = (point+ point (cons (* nx m)
                                                      (* ny m)))
                  for cell = nil
                  unless (point-in-bounds-p new-point map)
                    do (return 0)
                  do (setf cell (map-cell map new-point))
                  unless (char= cell char)
                    do (return 0)
                  finally (return 1))))

(defun mas-p (map point t-x)
  (let ((ps (list (point+ point (cons t-x -1))
                  (point+ point (cons (* t-x -1) 1)))))
    (unless (every (rcurry #'point-in-bounds-p map) ps)
      (return-from mas-p nil))
    (let ((cs (mapcar (curry #'map-cell map) ps)))
      (and (member #\M cs)
           (member #\S cs)))))

(defun x-mas-p (map point)
  (and (mas-p map point -1)
       (mas-p map point 1)))

(defun day-4 (input)
  (loop with map = (make-map input)
        with task-1 = 0
        with task-2 = 0
        for x from 0 below (input-map-width map)
        do (loop for y from 0 below (input-map-height map)
                 for point = (cons x y)
                 for elem = (map-cell map point)
                 when (char= elem #\X)
                   do (incf task-1 (count-xmas map point))
                 when (and (char= elem #\A)
                           (x-mas-p map point))
                   do (incf task-2))
        finally (return (values task-1 task-2))))
