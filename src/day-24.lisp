(defpackage #:aoc/day-24
  (:use #:cl #:aoc/utils)
  (:export #:day-24))
(in-package #:aoc/day-24)

(defstruct node
  name
  (value 0))

(defstruct gate
  type
  inputs
  output)

(defun parse-input (input)
  (let ((nodes (make-hash-table :test #'equal)))
    (loop for line = (read-line input)
          until (= (length line) 0)
          for colon = (position #\: line)
          for node = (make-node :name (subseq line 0 colon)
                                :value (parse-integer line :start (+ colon 2)))
          do (setf (gethash (node-name node) nodes) node))
    (values
     (hash-table-values nodes)
     (loop for line = (read-line input nil)
           until (null line)
           for parts = (uiop:split-string line :separator '(#\Space))
           for gate = (make-gate :type (eswitch ((nth 1 parts) :test #'string=)
                                         ("OR" #'logior)
                                         ("AND" #'logand)
                                         ("XOR" #'logxor))
                                 :inputs (list (ensure-gethash (nth 0 parts) nodes
                                                               (make-node :name (nth 0 parts)))
                                               (ensure-gethash (nth 2 parts) nodes
                                                               (make-node :name (nth 2 parts))))
                                 :output (ensure-gethash (nth 4 parts) nodes
                                                         (make-node :name (nth 4 parts))))
           collect gate)
     nodes)))

(defun ready-gates (nodes gates)
  (loop for gate in gates
        when (every (lambda (node)
                      (member node nodes))
                    (gate-inputs gate))
          collect gate))

(defun run-gate (gate)
  (setf (node-value (gate-output gate))
        (apply (gate-type gate)
               (mapcar #'node-value
                       (gate-inputs gate)))))

(defun prefix-nodes (nodes prefix)
  (sort (loop for node in nodes
              when (char= (aref (node-name node) 0) prefix)
                collect node)
        #'string>
        :key #'node-name))

(defun run-all-gates (nodes gates)
  (loop for ready = (ready-gates nodes gates)
        until (null ready)
        do (loop for gate in ready
                 do (run-gate gate)
                    (push (gate-output gate) nodes)
                    (setf gates (remove gate gates)))
        finally (return (prefix-nodes nodes #\z))))

(defun nodes-result (nodes)
  (loop with result = 0
        for node in nodes
        do (setf result (ash result 1)
                 result (logior result (node-value node)))
        finally (return result)))

(defun task-1 (nodes gates)
  (nodes-result (run-all-gates nodes gates)))

(defun reset-values (nodes)
  (loop for node in nodes
        for f = (aref (node-name node) 0)
        unless (or (char= f #\x)
                   (char= f #\y))
          do (setf (node-value node) 0)))

(defun find-gate-for-output (gates output)
  (find output gates
        :key #'gate-output))

(defun swap-outputs (gates output-1 output-2)
  (let ((gate-1 (find-gate-for-output gates output-1))
        (gate-2 (find-gate-for-output gates output-2)))
    (psetf (gate-output gate-1) (gate-output gate-2)
           (gate-output gate-2) (gate-output gate-1))))

(defun function-name (function)
  (cond
    ((eq function #'logand) "AND")
    ((eq function #'logior) "OR")
    ((eq function #'logxor) "XOR")
    (t "?")))

(defun swap-all (all-nodes gates pairs)
  (loop for pair in pairs
        for nodes = (mapcar (rcurry #'gethash all-nodes) pair)
        do (apply #'swap-outputs gates nodes)))

(defun task-2 (nodes gates)
  (swap-all nodes gates
            '(("mkk" "z10")
              ("qbw" "z14")
              ("wcb" "z34")
              ("wjb" "cvp")))
  (with-open-file (s #P"out.dot"
                     :direction :output
                     :if-exists :supersede)
    (format s "digraph {~%")
    (loop for gate in gates
          for gate-type = (function-name (gate-type gate))
          for gate-name = (format nil "~A_~A_~A"
                                  (node-name (first (gate-inputs gate)))
                                  gate-type
                                  (node-name (second (gate-inputs gate))))
          do (format s "~A[label=\"~A\"];~%" gate-name gate-type)
             (format s "~A -> ~A;~%" gate-name (node-name (gate-output gate)))
             (loop for input in (gate-inputs gate)
                   do (format s "~A -> ~A;~%" (node-name input) gate-name)))
    (format s "}~%"))
  "cvp,mkk,qbw,wcb,wjb,z10,z14,z34")

(defun day-24 (input)
  (multiple-value-bind (nodes gates all-nodes)
      (parse-input input)
    (values (task-1 nodes gates)
            (task-2 all-nodes gates))))
