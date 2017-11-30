; SMPSEQ8 - Fun with Sequences (Act 6)
; http://www.spoj.com/problems/SMPSEQ8/

(defvar *n* (read))
(defvar *s* (loop for i below *n* collect (read)))
(defvar *m* (read))
(defvar *q* (loop for i below *m* collect (read)))

(defvar *sum-s* (apply #'+ *s*))
(defvar *sum-q* (apply #'+ *q*))

(format t "~{~a~^ ~}~%" (if (< *sum-q* *sum-s*) *s* *q*))

