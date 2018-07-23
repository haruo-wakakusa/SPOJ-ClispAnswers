; CMPLS - Complete the Sequence!
; https://www.spoj.com/problems/CMPLS/

(defstruct testcase s c x-list)

(defun read-testcase ()
  (let ((s (read)))
    (make-testcase
      :s s
      :c (read)
      :x-list (loop for nil below s collect (read)))))
(compile 'read-testcase)

(defun make-difference-table (testcase)
  (let ((table (make-array (testcase-s testcase))))
    (setf (aref table 0) (reverse (testcase-x-list testcase)))
    (loop for i from 1 below (testcase-s testcase) do
      (setf (aref table i) (mapcar #'- (aref table (1- i))
                                       (rest (aref table (1- i))))))
  table))
(compile 'make-difference-table)

(defun expand-difference-table (testcase table)
  (setf (aref table (1- (testcase-s testcase)))
        (cons (first (aref table (1- (testcase-s testcase))))
              (aref table (1- (testcase-s testcase)))))
  (loop for i from (- (testcase-s testcase) 2) downto 0 do
    (setf (aref table i)
          (cons (+ (first (aref table i)) (first (aref table (1+ i))))
                (aref table i)))))
(compile 'expand-difference-table)

(dotimes (i (read))
  (let* ((testcase (read-testcase))
         (table (make-difference-table testcase)))
    (dotimes (j (testcase-c testcase))
      (expand-difference-table testcase table))
    (format t "~{~a~^ ~}~%"
      (reverse (subseq (aref table 0) 0 (testcase-c testcase))))))

