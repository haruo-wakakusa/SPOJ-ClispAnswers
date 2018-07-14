; bulk

;  7 6
; 4 5  y
;  3 2  z
; 0 1  + x

(defun get-point (min max pos)
  (ccase pos
    (0 (list min min min)) (1 (list max min min))
    (2 (list max min max)) (3 (list min min max))
    (4 (list min max min)) (5 (list max max min))
    (6 (list max max max)) (7 (list min max max))))

(defun get-plane (min max indices)
  (cons 4 (apply #'append
                 (mapcar (lambda (pos) (get-point min max pos))
                         indices))))

(defun write-box (min max)
  (dolist (indices (list '(0 1 2 3) ; bottom
                         '(4 5 6 7) ; up
                         '(0 1 5 4) ; front
                         '(3 2 6 7) ; back
                         '(0 3 7 4) ; left
                         '(1 2 6 5) ; right
          ))
    (format t "~{~a~^ ~}~%"
      (get-plane min max indices))))

(defvar *t* 1)
(format t "~a~%" *t*)

(defvar *boxes* 10)
(format t "~a~%" (* 6 *boxes*))
(dotimes (i *boxes*) (write-box i (- 1000 i)))

