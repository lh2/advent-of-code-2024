(defpackage #:aoc/day-17
  (:use #:cl #:aoc/utils)
  (:export
   #:*use-compiler*
   #:make-registers
   #:make-program
   #:interpret
   #:compile-program
   #:day-17))
(in-package #:aoc/day-17)

(defparameter *use-compiler* nil)

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

(declaim (inline adv bxl bst bxc bdv cdv)
         (ftype (function ((simple-array fixnum (3)) fixnum) null) adv bxl bst bxc bdv cdv))

(defun adv (registers operand)
  (setf (aref registers 0)
        (floor (aref registers 0)
               (the fixnum (expt 2 (the (integer 0 61)
                                        (combo-operand operand registers))))))
  nil)

(defun bxl (registers operand)
  (setf (aref registers 1)
        (logxor (aref registers 1)
                operand))
  nil)

(defun bst (registers operand)
  (setf (aref registers 1)
        (mod (combo-operand operand registers) 8))
  nil)

(defun bxc (registers operand)
  (declare (ignore operand))
  (setf (aref registers 1)
        (logxor (aref registers 1)
                (aref registers 2)))
  nil)

(defun bdv (registers operand)
  (setf (aref registers 1)
        (floor (aref registers 0)
               (the fixnum (expt 2 (the (integer 0 61)
                                        (combo-operand operand registers))))))
  nil)

(defun cdv (registers operand)
  (setf (aref registers 2)
        (floor (aref registers 0)
               (the fixnum (expt 2 (the (integer 0 61)
                                        (combo-operand operand registers))))))
  nil)

(defun make-jump (jump-tags registers op)
  `(when (not (zerop (aref ,registers 0)))
     (go ,(aref jump-tags op))))

(defun make-output (output registers op)
  `(push (mod (combo-operand ,op ,registers) 8) ,output))

(defun compile-program (program)
  (let* ((registers (gensym "REGISTERS"))
         (jump-tags (coerce
                     (loop repeat (/ (length program) 2)
                           for i from 0
                           collect (gensym (format nil "INST-~A" i)))
                     'vector))
         (output (gensym "OUTPUT"))
         (code (loop for i from 0 below (length program) by 2
                     for inst-id from 0
                     for inst = (aref program i)
                     for op = (aref program (1+ i))
                     collect (aref jump-tags inst-id)
                     collect (case inst
                               (0 `(adv ,registers ,op))
                               (1 `(bxl ,registers ,op))
                               (2 `(bst ,registers ,op))
                               (3 (make-jump jump-tags registers op))
                               (4 `(bxc ,registers ,op))
                               (5 (make-output output registers op))
                               (6 `(bdv ,registers ,op))
                               (7 `(cdv ,registers ,op))))))
    (compile nil
             `(lambda (,registers)
                (declare (optimize speed)
                         (type (simple-array fixnum (3)) ,registers))
                (let ((,output nil))
                  (tagbody
                     ,@code)
                  ,output)))))

(declaim (ftype (function ((simple-array fixnum (3)) simple-vector) list) interpret))

(defun interpret (registers code)
  (loop with output = nil
        for ip from 0 below (length code) by 2
        for instruction = (aref code ip)
        for operand = (aref code (1+ ip))
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

(defun task-2 (registers program code)
  (let ((original-registers (copy-seq registers)))
    (labels ((%solve (register-a depth)
               (loop for n from 0 below 8
                     for nra = (+ register-a n)
                     do (reset-registers registers original-registers nra)
                        (let* ((res (funcall program registers))
                               (value (nth depth res)))
                          (when (= value (aref code (- (length code) 1 depth)))
                            (when (= depth (1- (length code)))
                              (return-from %solve nra))
                            (let ((solved-register-a (%solve (* nra 8) (1+ depth))))
                              (when solved-register-a
                                (return-from %solve solved-register-a))))))))
      (%solve 1 0))))

(defun make-program (code)
  (if *use-compiler*
      (compile-program code)
      (lambda (registers)
        (interpret registers code))))

(defun day-17 (input)
  (multiple-value-bind (registers code)
      (parse-input input)
    (let ((program (make-program code)))
      (values
       (format nil "~{~A~^,~}" (nreverse (funcall program (copy-seq registers))))
       (task-2 registers program code)))))
