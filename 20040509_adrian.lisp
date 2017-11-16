; FCTRL - Factorial
; http://www.spoj.com/problems/FCTRL/

(defun solve-z-value (n)
  (do ((pow5 5 (* 5 pow5)) (sum 0)) ((> pow5 n) sum)
    (setf sum (+ sum (floor (/ n pow5))))))

(defvar *t* (read))
(dotimes (i *t*) (print (solve-z-value (read))))

