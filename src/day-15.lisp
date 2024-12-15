(defpackage #:aoc/day-15
  (:use #:cl #:aoc/utils)
  (:export
   #:follow-directions
   #:day-15))
(in-package #:aoc/day-15)

(defparameter *moves* '(:up   (0 . -1) :down  (0 . 1)
                        :left (-1 . 0) :right (1 . 0)))

(defun read-directions (input)
  (loop for line = (read-line input nil)
        until (null line)
        nconc (mapcar (lambda (c)
                        (ecase c
                          (#\^ :up)
                          (#\< :left)
                          (#\> :right)
                          (#\v :down)))
                      (coerce line 'list))))

(defun expand-map (map)
  (loop with width = (* (input-map-width map) 2)
        with height = (input-map-height map)
        with new-map = (make-empty-map width height)
        for y from 0 below (input-map-height map)
        do (loop for x from 0 below (input-map-width map)
                 for point = (cons x y)
                 for new-point-left = (cons (* x 2) y)
                 for new-point-right = (cons (+ (* x 2) 1) y)
                 for cell = (map-cell map point)
                 unless (char= cell #\.)
                   do (setf (map-cell new-map new-point-left)
                            (ecase cell
                              (#\# #\#)
                              (#\O #\[))
                            (map-cell new-map new-point-right)
                            (ecase cell
                              (#\# #\#)
                              (#\O #\]))))
        finally (return new-map)))

(defun find-robot (map)
  (loop for y from 0 below (input-map-height map)
        thereis (loop for x from 0 below (input-map-width map)
                      for pos = (cons x y)
                      when (char= #\@ (map-cell map pos))
                        do (return pos))))

(defun move-boxes-basic (map pos direction)
  (let* ((cell (map-cell map pos))
         (diff (getf *moves* direction))
         (next (point+ pos diff)))
    (case cell
      (#\. (return-from move-boxes-basic t))
      (#\# (return-from move-boxes-basic nil)))
    (when (move-boxes map next direction)
      (setf (map-cell map next) cell
            (map-cell map pos) #\.)
      t)))

(defun box-positions (map pos)
  (case (map-cell map pos)
    (#\[ (list pos (point+ pos '(1 . 0))))
    (#\] (list pos (point- pos '(1 . 0))))))

(defun clean-positions (positions)
  (remove-duplicates (remove nil positions) :test #'equal))

(defun move-boxes (map pos direction)
  (case (map-cell map pos)
    (#\. (return-from move-boxes t))
    (#\O (return-from move-boxes
           (move-boxes-basic map pos direction)))
    (#\# (return-from move-boxes nil)))
  (when (member direction '(:left :right))
    (return-from move-boxes (move-boxes-basic map pos direction)))
  (loop with diff = (getf *moves* direction)
        with positions = (box-positions map pos)
        with to-be-moved = (list positions)
        for y = (point-y pos) then (+ y (point-y diff))
        do (setf positions
                 (clean-positions
                  (loop for position in positions
                        for next = (point+ position diff)
                        nconc (case (map-cell map next)
                                (#\# (return-from move-boxes nil))
                                (#\. nil)
                                (t (box-positions map next))))))
           (push positions to-be-moved)
        until (null positions)
        finally (loop for set in to-be-moved
                      do (loop for position in set
                               for next = (point+ position diff)
                               do (setf (map-cell map next) (map-cell map position)
                                        (map-cell map position) #\.))))
  t)

(defun all-boxes-gps-coords (map)
  (loop for y from 0 below (input-map-height map)
        sum (loop for x from 0 below (input-map-width map)
                  for pos = (cons x y)
                  for cell = (map-cell map pos)
                  when (or (char= cell #\O)
                           (char= cell #\[))
                    sum (+ (* y 100) x))))

(defun follow-directions (map pos directions)
  (loop for direction in directions
        for i from 0
        for new-pos = (point+ pos (getf *moves* direction))
        when (move-boxes map new-pos direction)
          do (setf pos new-pos)))

(defun day-15 (input)
  (let* ((map-1 (make-map input))
         (pos (find-robot map-1))
         (map-2 (progn
                  (setf (map-cell map-1 pos) #\.)
                  (expand-map map-1)))
         (directions (read-directions input)))
    (follow-directions map-1 pos directions)
    (follow-directions map-2 (point* pos (cons 2 1)) directions)
    (values (all-boxes-gps-coords map-1)
            (all-boxes-gps-coords map-2))))
