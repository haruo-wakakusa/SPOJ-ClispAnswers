; SMPSEQ9 - Fun with Sequences (Act 7)
; http://www.spoj.com/problems/SMPSEQ9/

(defvar *n* (read))
(defvar *s* (loop for _ below *n* collect (read)))
(defvar *m* (read))
(defvar *q* (loop for _ below *m* collect (read)))

(format t "~{~a~^ ~}~%"
  (if (< (/ (apply #'+ *q*) *m*) (/ (apply #'+ *s*) *n*)) *s* *q*))

