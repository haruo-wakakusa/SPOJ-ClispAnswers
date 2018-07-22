; BULK - The Bulk!
; https://www.spoj.com/problems/BULK/

(defstruct point (x) (y) (z))
(defstruct yz-pt (y) (z))

(defun read-testcase ()
  (loop for nil below (read) collect
    (loop for nil below (read) collect (list (read) (read) (read)))))

(defun get-points (faces)
  (apply #'append faces))
