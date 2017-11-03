; sqrt.li

(defvar x (read))

(when (< x 0.0)
  (print "Input positive real number.")
  (exit))

(defvar EPS 0.00001)

(defun solve-sqrt (min max)
  (if (< (abs (- max min)) EPS)
      min
      (let ((mid (/ (+ min max) 2.0)))
        (if (< x (* mid mid))
            (solve-sqrt min mid)
            (solve-sqrt mid max)))))

(print (solve-sqrt 0.0 (max x 1.0)))

