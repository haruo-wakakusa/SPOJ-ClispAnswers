; SBSTR1 - Substring Check (Bug Funny)
; http://www.spoj.com/problems/SBSTR1/

(defconstant +ary-size+ 20)
(defconstant +prg-size+ 10000)

; brainfuck environment

(defvar *ary* (make-array (list +ary-size+)))
(defvar *ptr* 0)

(defvar *prg* (make-array (list +prg-size+)))
(defvar *prg-len* 0)
(defvar *pc* 0)

; brainfuck instructions

(defun bf-ptr-incr ()
  (incf *ptr*)
  (when (= *ptr* +ary-size+) (error "pointer overflow.")))

(defun bf-ptr-decr ()
  (decf *ptr*)
  (when (< *ptr* 0) (error "pointer underflow.")))

(defun bf-incr ()
  (incf (aref *ary* *ptr*))
  (when (= (aref *ary* *ptr*) 256) (setf (aref *ary* *ptr*) 0)))

(defun bf-decr ()
  (decf (aref *ary* *ptr*))
  (when (= (aref *ary* *ptr*) -1) (setf (aref *ary* *ptr*) 255)))

(defun bf-print ()
  (write-char (code-char (aref *ary* *ptr*))))

(defun bf-read ()
  (setf (aref *ary* *ptr*) (char-code (read-char))))

(defun bf-pc-incr ()
  (incf *pc*)
  (when (= *pc* *prg-len*) (error "program counter overflow.")))

(defun bf-pc-decr ()
  (decf *pc*)
  (when (< *pc* 0) (error "program counter underflow.")))

(defun bf-loop ()
  (when (zerop (aref *ary* *ptr*))
    (block nil (loop
      (bf-pc-incr)
      (when (char= (aref *prg* *pc*) #\]) (return))))))

(defun bf-loopend ()
  (block nil (loop
    (bf-pc-decr)
    (when (char= (aref *prg* *pc*) #\[) (return))))
  (decf *pc*)) ; it is right due to run-step increment

; interpreter

(defun init ()
  (dotimes (i +ary-size+) (setf (aref *ary* i) 0))
  (setf *ptr* 0 *pc* 0))

(defun run-step ()
  (assert (and (>= *pc* 0) (<= *pc* *prg-len*)))
  (when (= *pc* *prg-len*) (return-from run-step :end))
  (case (aref *prg* *pc*)
    (#\> (bf-ptr-incr)) (#\< (bf-ptr-decr))
    (#\+ (bf-incr)) (#\- (bf-decr))
    (#\. (bf-print)) (#\, (bf-read))
    (#\[ (bf-loop)) (#\] (bf-loopend)))
  (incf *pc*)
  :continue)

(defun run ()
  (init)
  (block nil (loop
    (when (eq (run-step) :end) (return)))))

(defun prg-load (str)
  (assert (<= (length str) +prg-size+))
  (setf *prg-len* (length str))
  (with-input-from-string (s str)
    (dotimes (i *prg-len*) (setf (aref *prg* i) (read-char s)))))

; brainfuck script writer

(defun prg-init ()
  (init)
  (setf *prg-len* 0))

(defun prg-add (c)
  (assert (< *prg-len* +prg-size+))
  (setf (aref *prg* *prg-len*) c)
  (incf *prg-len*))

(defun goto (i)
  (block nil (loop
    (cond ((< *ptr* i) (prg-add #\>) (incf *ptr*))
          ((> *ptr* i) (prg-add #\<) (decf *ptr*))
          (t (return))))))

(defun assign-zero ()
  (prg-add #\[) (prg-add #\-) (prg-add #\]))

(defun add-immediate (n)
  (dotimes (i n) (prg-add #\+)))

(defun sub-immediate (n)
  (dotimes (i n) (prg-add #\-)))

(defun add-if-one (cond-i dst) :TODO-NOT-IMPLEMENTED )

(defun prg-print ()
  (format t "~A~%" (map 'string #'identity (subseq *prg* 0 *prg-len*))))

; brainfuck script

(defconstant +char-zero+ (char-code #\0))

(prg-init)

(loop for i from 0 below 10 do
  (goto i)
  (prg-add #\,)
  (sub-immediate +char-zero+))

(goto 10)
(prg-add #\,) ; void read

(loop for i from 10 below 15 do
  (goto i)
  (prg-add #\,)
  (sub-immediate +char-zero+))

