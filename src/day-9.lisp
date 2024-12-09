(defpackage #:aoc/day-9
  (:use #:cl #:aoc/utils)
  (:export #:day-9))
(in-package #:aoc/day-9)

(defun repeat (item n)
  (loop repeat n collect item))

(defun parse-hdd (input)
  (loop for cf = (read-char input nil)
        for ce = (read-char input nil)
        for file-id from 0
        until (null cf)
        nconc (repeat file-id (char-number cf)) into hdd
        unless (null ce)
          nconc (repeat nil (char-number ce)) into hdd
        finally (return (values (coerce hdd 'vector)
                                (1- file-id)))))

(defun not-eql (a b)
  (not (eql a b)))

(defun compact (hdd)
  (loop for empty = (position nil hdd :start (or empty 0))
        for block = (position nil hdd
                              :test #'not-eql
                              :from-end t
                              :end (or block nil))
        until (or (or (null empty) (null block))
                  (< block empty))
        do (setf (aref hdd empty) (aref hdd block)
                 (aref hdd block) nil)
        finally (return hdd)))

(defun find-empty-space (hdd length end)
  (declare (optimize debug))
  (loop for empty-start = (position nil hdd
                                    :start (or empty-start 0)
                                    :end end)
        for empty-length = (and empty-start
                                (- (or (position nil hdd
                                                 :test #'not-eql
                                                 :start empty-start)
                                       (length hdd))
                                   empty-start))
        until (null empty-start)
        when (>= empty-length length)
          do (return empty-start)
        do (setf empty-start (position nil hdd
                                       :test #'not-eql
                                       :start empty-start
                                       :end end))
        when (null empty-start)
          do (return nil)))

(defun defrag (hdd max-file-id)
  (loop for file-id from max-file-id downto 0
        for pos-end = (position file-id hdd
                                :from-end t)
        for pos-start = (1+ (or (position file-id hdd
                                          :test #'not-eql
                                          :from-end t
                                          :end pos-end)
                                -1))
        for block-length = (1+ (- pos-end pos-start))
        for fitting-space = (find-empty-space hdd block-length pos-start)
        when fitting-space
          do (loop repeat block-length
                   for ep from fitting-space
                   for bp from pos-start
                   do (setf (aref hdd ep) (aref hdd bp)
                            (aref hdd bp) nil))
        finally (return hdd)))

(defun calculate-checksum (hdd)
  (loop for id across hdd
        for pos from 0
        unless (null id)
          sum (* pos id)))

(defun day-9 (input)
  (multiple-value-bind (hdd max-file-id)
      (parse-hdd input)
    (values (calculate-checksum (compact (copy-seq hdd)))
            (calculate-checksum (defrag (copy-seq hdd) max-file-id)))))
