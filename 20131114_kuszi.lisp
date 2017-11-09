; SMPSEQ3 - Fun with Sequences
; http://www.spoj.com/problems/SMPSEQ3/

(defvar *n* (read))
(defvar *s* (loop for i below *n* collect (read)))
(defvar *m* (read))
(defvar *q* (loop for i below *m* collect (read)))

(defun difference-set (a b)
  (let ((res nil))
    (block nil
      (let ((rest-a a) (rest-b b) (a1 (car a)) (b1 (car b)))
        (loop
          (when (null rest-a) (return))
          (cond ((null rest-b)  (setf res (cons a1 res) rest-a (cdr rest-a)))
                ((= a1 b1) (setf rest-a (cdr rest-a) rest-b (cdr rest-b)))
                ((< a1 b1) (setf res (cons a1 res) rest-a (cdr rest-a)))
                (t         (setf rest-b (cdr rest-b))))
          (setf a1 (car rest-a) b1 (car rest-b)))))
    (reverse res)))

(defun print-with-space (lst)
  (let ((l lst) (last (last lst)))
    (block nil (loop
      (when (eq l last) (return))
      (format *standard-output* "~d " (car l))
      (setf l (cdr l))))
    (format *standard-output* "~d~%" (car last))))

(print-with-space (difference-set *s* *q*))

