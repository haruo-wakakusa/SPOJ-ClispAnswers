; CMPLS - Complete the Sequence!
; https://www.spoj.com/problems/CMPLS/

; this version exceeds the time limit.

(defstruct testcase s c x-list)

(defun read-testcase ()
  (let ((s (read)))
    (make-testcase
      :s s
      :c (read)
      :x-list (loop for nil below s collect (read)))))

(defun make-gauss-matrix (testcase)
  (let ((mat (make-array (list (testcase-s testcase)
                               (1+ (testcase-s testcase))))))
    (loop for i below (testcase-s testcase) do
      (loop for j below (testcase-s testcase) do
        (setf (aref mat i j) (expt (1+ i) j)))
      (setf (aref mat i (testcase-s testcase))
            (nth i (testcase-x-list testcase))))
    mat))

(defun matrix-row-mult (mat testcase row magnitude)
  (loop for j to (testcase-s testcase) do
    (setf (aref mat row j) (* magnitude (aref mat row j)))))

(defun matrix-row-add (mat testcase dst-row src-row delta)
  (loop for j to (testcase-s testcase) do
    (incf (aref mat dst-row j) (* (aref mat src-row j) delta))))

(defun row-reduction (mat testcase row)
  (matrix-row-mult mat testcase row (/ (aref mat row row)))
  (loop for i below (testcase-s testcase) unless (= i row) do
    (matrix-row-add mat testcase i row (- (aref mat i row)))))

(defun get-polynomial-value (mat testcase n)
  (loop for j below (testcase-s testcase) sum
    (* (aref mat j (testcase-s testcase)) (expt n j))))

(defun solve-testcase ()
  (let* ((testcase (read-testcase))
         (matrix (make-gauss-matrix testcase)))
    (loop for i below (testcase-s testcase) do
      (row-reduction matrix testcase i))
    (format t "~{~a~^ ~}~%"
      (mapcar (lambda (n) (get-polynomial-value matrix testcase n))
              (loop for i
                    from (1+ (testcase-s testcase))
                    to (+ (testcase-s testcase) (testcase-c testcase))
                    collect i)))))

(dotimes (i (read)) (solve-testcase))

