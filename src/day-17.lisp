(defpackage #:aoc/day-17
  (:use #:cl #:aoc/utils)
  (:export
   #:make-registers
   #:run-program
   #:day-17))
(in-package #:aoc/day-17)

(defun parse-register (line)
  (parse-integer line :start (1+ (position #\: line))))

(defun make-registers (a &optional (b 0) (c 0))
  (make-array 3
              :element-type 'fixnum
              :initial-contents (list a b c)))

(defun parse-input (input)
  (let ((a (parse-register (read-line input)))
        (b (parse-register (read-line input)))
        (c (parse-register (read-line input)))
        (program-line (progn
                        (read-line input)
                        (read-line input))))
    (values (make-registers a b c)
            (coerce (read-number-list program-line :start (1+ (position #\: program-line)))
                    'vector))))

(declaim (inline combo-operand)
         (ftype (function (fixnum (simple-array fixnum (3))) fixnum) combo-operand))

(defun combo-operand (operand registers)
  (if (<= operand 3)
      operand
      (if (<= operand 7)
          (aref registers (- operand 4))
          (error "Invalid operand ~A for combo operator" operand))))

(defun run-program (registers program)
  (loop with output = nil
        for ip from 0 below (length program) by 2
        for instruction = (aref program ip)
        for operand = (aref program (1+ ip))
        do (case instruction
             (0 (setf (aref registers 0)
                      (floor (aref registers 0)
                             (expt 2 (combo-operand operand registers)))))
             (1 (setf (aref registers 1)
                      (logxor (aref registers 1)
                              operand)))
             (2 (setf (aref registers 1)
                      (mod (combo-operand operand registers) 8)))
             (3 (when (not (zerop (aref registers 0)))
                  (setf ip (- operand 2))))
             (4 (setf (aref registers 1)
                      (logxor (aref registers 1)
                              (aref registers 2))))
             (5 (push (mod (combo-operand operand registers) 8) output))
             (6 (setf (aref registers 1)
                      (floor (aref registers 0)
                             (expt 2 (combo-operand operand registers)))))
             (7 (setf (aref registers 2)
                      (floor (aref registers 0)
                             (expt 2 (combo-operand operand registers))))))
        finally (return output)))

(defun reset-registers (registers original-registers register-a)
  (setf (aref registers 0) register-a
        (aref registers 1) (aref original-registers 1)
        (aref registers 2) (aref original-registers 2)))

(defun task-2 (registers code)
  (let ((original-registers (copy-seq registers)))
    (labels ((%solve (register-a depth)
               (loop for n from 0 below 8
                     for nra = (+ register-a n)
                     do (reset-registers registers original-registers nra)
                        (let* ((res (run-program registers code))
                               (value (nth depth res)))
                          (when (= value (aref code (- (length code) 1 depth)))
                            (when (= depth (1- (length code)))
                              (return-from %solve nra))
                            (let ((solved-register-a (%solve (* nra 8) (1+ depth))))
                              (when solved-register-a
                                (return-from %solve solved-register-a))))))))
      (%solve 1 0))))

(defun day-17 (input)
  (multiple-value-bind (registers code)
      (parse-input input)
    (values
     (format nil "~{~A~^,~}" (nreverse (run-program (copy-seq registers) code)))
     (task-2 registers code))))
