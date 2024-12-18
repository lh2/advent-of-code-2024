(defpackage #:aoc/day-18
  (:use #:cl #:aoc/utils)
  (:export
   #:*width*
   #:*height*
   #:*bytes-falling*
   #:day-18))
(in-package #:aoc/day-18)

(defparameter *width* 71)
(defparameter *height* 71)
(defparameter *bytes-falling* 1024)
(defparameter *directions* '((1 . 0) (-1 . 0)
                             (0 . 1) (0 . -1)))

(defun parse-coordinate (line)
  (multiple-value-bind (x end)
      (parse-integer line :junk-allowed t)
    (cons x (parse-integer line :start (1+ end)))))

(defun parse-coordinates (input)
  (loop for line = (read-line input nil)
        for i from 0
        until (null line)
        collect (parse-coordinate line)))

(defun fall-byte (map position)
  (setf (map-cell map position) #\#))

(defun draw-map (map visited)
  (loop for y from 0 below *height*
        do (loop for x from 0 below *width*
                 for pos = (cons x y)
                 do (format t "~A" (or (and (gethash pos visited) #\O)
                                       (map-cell map pos))))
           (format t "~%")))

(defun bfs (map)
  (loop with queue = (make-queue :simple-queue)
        with start = (cons 0 0)
        with end = (cons (1- *width*) (1- *height*))
        with visited = (make-hash-table :test #'equal)
        initially (qpush queue (list start 0))
                  (setf (gethash start visited) t)
        for (pos steps) = (qpop queue)
        when (null pos)
          do (return nil)
        when (equal pos end)
          do (return steps)
        do (loop for dir in *directions*
                 for next = (point+ pos dir)
                 when (and (point-in-bounds-p next *width* *height*)
                           (not (gethash next visited))
                           (char/= (map-cell map next) #\#))
                   do (qpush queue (list next (1+ steps)))
                      (setf (gethash next visited) t))))

(defun task-2 (map coordinates)
  (loop for byte in coordinates
        do (fall-byte map byte)
        unless (bfs map)
          do (return byte)))

(defun day-18 (input)
  (let* ((map (make-empty-map *width* *height*))
         (coordinates (parse-coordinates input)))
    (loop repeat *bytes-falling*
          for cdr on coordinates
          do (fall-byte map (car cdr))
          finally (setf coordinates cdr))
    (values (bfs map)
            (let ((p (task-2 map coordinates)))
              (format nil "~A,~A" (point-x p) (point-y p))))))
