(defpackage #:aoc/day-16
  (:use #:cl #:aoc/utils)
  (:export #:day-16))
(in-package #:aoc/day-16)

(defparameter *directions-clockwise* '(#1=(1 . 0) (0 . 1)
                                       (-1 . 0) (0 . -1) #1#))
(defparameter *directions-counterclockwise* '(#2=(1 . 0) (0 . -1)
                                              (-1 . 0) (0 . 1) #2#))

(defstruct node
  position
  direction
  (cost 0)
  (parents nil))

(defun node-compare (node-a node-b)
  (< (node-cost node-a) (node-cost node-b)))

(defun process-next (open-list node-cache closed-list next-position next-direction next-cost parent)
  (when (gethash (list next-position next-direction) closed-list)
    (return-from process-next nil))
  (let* ((cache-key (list next-direction next-position))
         (existing (gethash cache-key node-cache)))
    (if existing
        (when (<= next-cost (node-cost existing))
          (setf (node-parents existing) (if (= next-cost (node-cost existing))
                                            (cons parent (node-parents existing))
                                            (list parent))
                (node-cost existing) next-cost))
        (qpush open-list (setf (gethash cache-key node-cache)
                               (make-node :position next-position
                                          :direction next-direction
                                          :cost next-cost
                                          :parents (list parent)))))))

(defun path-length (end-node)
  (loop with seen = (make-hash-table :test #'equal)
        for nodes = (list end-node) then (mappend #'node-parents nodes)
        until (null nodes)
        do (loop for node in nodes
                 do (setf (gethash (node-position node) seen) t))
        finally (return (hash-table-count seen))))

(defun dijkstra (map start end)
  (loop with open-list = (make-queue :priority-queue :compare #'node-compare)
        with node-cache = (make-hash-table :test #'equal)
        with closed-list = (make-hash-table :test #'equal)
        initially (qpush open-list (make-node :position start
                                              :direction (first *directions-clockwise*)))
        while (> (qsize open-list) 0)
        for current = (qpop open-list)
        for current-pos = (node-position current)
        for current-dir = (node-direction current)
        for current-cost = (node-cost current)
        for next = (point+ current-pos current-dir)
        when (equal current-pos end)
          do (return (values current-cost
                             (path-length current)))
        do (setf (gethash (list current-pos current-dir) closed-list) t)
        when (char/= (map-cell map next) #\#)
          do (process-next open-list node-cache closed-list next current-dir (1+ current-cost) current)
        do (process-next open-list node-cache closed-list current-pos
                         (cadr (member current-dir *directions-clockwise* :test #'equal))
                         (+ current-cost 1000)
                         current)
           (process-next open-list node-cache closed-list current-pos
                         (cadr (member current-dir *directions-counterclockwise* :test #'equal))
                         (+ current-cost 1000)
                         current)))

(defun day-16 (input)
  (let* ((map (make-map input))
         (start (map-find map #\S))
         (end (map-find map #\E)))
    (dijkstra map start end)))
