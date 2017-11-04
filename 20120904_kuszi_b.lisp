; CPTTRN7 - Character Patterns (Act 7)
; http://www.spoj.com/problems/CPTTRN7/

(defconstant *array-max-size* 128)
(defvar *ary* (make-array (list *array-max-size*)))

(defun write-diamond-slice (row size)
  (dotimes (i (* 2 size)) (setf (aref *ary* i) #\.))
  (setf (aref *ary* (mod (- size 1 row) (* 2 size))) #\/)
  (setf (aref *ary* (mod (+ size row) (* 2 size))) #\\))

(defun copy-diamond-slice (col size)
  (let ((end (* col size 2)))
    (do ((i 0 (1+ i)) (j (* 2 size) (1+ j))) ((>= j end))
      (setf (aref *ary* j) (aref *ary* i)))))

(defun print-diamond-line (col size)
  (dotimes (i (* 2 size))
    (write-diamond-slice i size)
    (copy-diamond-slice col size)
    (write-sequence *ary* *standard-output* :end (* col size 2))
    (write-char #\Newline)))

(defun print-diamond (row col size)
  (dotimes (i row) (print-diamond-line col size)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-diamond (read) (read) (read))
  (write-char #\Newline))
(print-diamond (read) (read) (read))


