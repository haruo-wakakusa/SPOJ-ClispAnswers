(defvar *n* (read))
(defvar *s* (loop for i below *n* collect (read)))
(defvar *m* (read))
(defvar *q* (loop for i below *m* collect (read)))

(setf *res* nil)

(let ((s *s*) (q *q*))
  (loop for i from 1 to (min *n* *m*) do
    (when (= (car s) (car q)) (setq *res* (cons i *res*)))
    (setf s (cdr s) q (cdr q))))

(format t "~{~a~^ ~}~%" (reverse *res*))

