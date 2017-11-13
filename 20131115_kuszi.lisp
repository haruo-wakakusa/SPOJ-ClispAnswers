; SMPSEQ6 - Fun with Sequences (Act 4)
; http://www.spoj.com/problems/SMPSEQ6/

(defvar *n* (read))
(defvar *x* (read))
(defvar *s* (make-array (list *n*)))
(defvar *q* (make-array (list *n*)))
(dotimes (i *n*) (setf (aref *s* i) (read)))
(dotimes (i *n*) (setf (aref *q* i) (read)))

(defvar *res* (make-array (list *n*)))

(loop for y from (- *x*) to *x* do
  (loop for i from (max 0 (- y)) to (- *n* 1 (max 0 y)) do
    (when (= (aref *s* i) (aref *q* (+ i y))) (setf (aref *res* i) t))))

(format t "~{~a~^ ~}~%"
  (remove-if-not (lambda (i) (aref *res* (1- i)))
                 (loop for i from 1 to *n* collect i)))

