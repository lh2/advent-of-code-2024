(defpackage #:aoc/day-23
  (:use #:cl #:aoc/utils)
  (:export #:day-23))
(in-package #:aoc/day-23)

(defstruct computer
  name
  connected)

(defun computer-connected-length (computer)
  (length (computer-connected computer)))

(defun parse-computers (input)
  (loop with computers = (make-hash-table :test #'equal)
        for line = (read-line input nil)
        until (null line)
        for dash = (position #\- line)
        for n-1 = (subseq line 0 dash)
        for n-2 = (subseq line (1+ dash))
        for c-1 = (ensure-gethash n-1 computers (make-computer :name n-1))
        for c-2 = (ensure-gethash n-2 computers (make-computer :name n-2))
        do (pushnew c-1 (computer-connected c-2))
           (pushnew c-2 (computer-connected c-1))
        finally (return (hash-table-values computers))))

(defun clique-equal (clique-1 clique-2)
  (and (length= clique-1 clique-2)
       (length= clique-1 (intersection clique-1 clique-2))))

(defun find-cliques (computers)
  (labels ((%pivot (p x)
             (let ((all (union p x)))
               (first (sort all #'> :key #'computer-connected-length))))
           (%bron-kerbosch2 (r p x)
             (if (and (null p) (null x))
                 (list r)
                 (loop with u = (%pivot p x)
                       for v in (set-difference p (computer-connected u))
                       for c = (computer-connected v)
                       nconc (%bron-kerbosch2
                              (cons v r)
                              (intersection c p)
                              (intersection c x))))))
    (remove-duplicates (%bron-kerbosch2 nil computers nil) :test #'clique-equal)))

(defun possible-historian-computer-p (computer)
  (string-prefix-p "t" (computer-name computer)))

(defun password (computers)
  (format nil "~{~A~^,~}" (sort (mapcar #'computer-name computers) #'string<)))

(defun day-23 (input)
  (loop with computers = (parse-computers input)
        with seen = (make-hash-table :test #'equal)
        with task-1 = 0
        with largest-clique-length = 0
        with largest-clique = nil
        for clique in (find-cliques computers)
        for clique-length = (length clique)
        when (>= clique-length 3)
          do (map-combinations (lambda (combo)
                                 (setf combo (sort combo #'string< :key #'computer-name))
                                 (when (and (not (gethash combo seen))
                                            (some #'possible-historian-computer-p combo))
                                   (incf task-1)
                                   (setf (gethash combo seen) t)))
                               clique
                               :length 3)
        when (> clique-length largest-clique-length)
          do (setf largest-clique-length clique-length
                   largest-clique clique)
        finally (return (values task-1 (password largest-clique)))))
