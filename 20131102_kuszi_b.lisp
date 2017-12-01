; SPTTRN3 - Straight Line Spiral Pattern (Act 3)
; http://www.spoj.com/problems/SPTTRN3/

(defstruct pt (x 0) (y 0))

(defun pt+ (a b)
  (make-pt :x (+ (pt-x a) (pt-x b)) :y (+ (pt-y a) (pt-y b))))
(defun pt- (a b)
  (make-pt :x (- (pt-x a) (pt-x b)) :y (- (pt-y a) (pt-y b))))
(defmacro pt-ref (ary pt) `(aref ,ary (pt-y ,pt) (pt-x ,pt)))

(defconstant +r1+ (make-pt :x 1 :y 0)) ; right
(defconstant +r2+ (make-pt :x 1 :y 0))
(defconstant +r3+ (make-pt :x 1 :y 0))
(defconstant +d1+ (make-pt :x 0 :y 1)) ; down
(defconstant +d2+ (make-pt :x 0 :y 1))
(defconstant +d3+ (make-pt :x 0 :y 1))
(defconstant +l1+ (make-pt :x -1 :y 0)) ; left
(defconstant +l2+ (make-pt :x -1 :y 0))
(defconstant +u1+ (make-pt :x 0 :y -1)) ; up
(defconstant +u2+ (make-pt :x 0 :y -1))

(defconstant +rotation+
  '(+r1+ +d1+ +l1+ +u1+ +r2+ +d2+ +r3+ +u2+ +l2+ +d3+ +r1+))

(defun get-next-direction (dir)
  (let ((d +rotation+))
    (block nil (loop
      (when (eq (car d) dir) (return (cadr d)))
      (setf d (car d))))))

(defun make-test-case-array (size)
  (let ((ary (make-array (list (+ (pt-y size) 4) (+ (pt-x size) 4)))))
    (dotimes (i (+ (pt-y size) 4))
      (dotimes (j (+ (pt-x size) 4))
        (setf (aref ary i j) #\.)))
    (dotimes (i (+ (pt-y size) 4))
      (setf (aref ary i 0) #\*
            (aref ary i (+ (pt-x size) 3)) #\*))
    (dotimes (j (+ (pt-x size) 4))
      (setf (aref ary 0 j) #\*
            (aref ary (+ (pt-y size) 3) j) #\*))
    ary))

(defun walk-asap (ary init dir)
  (do* ((p init (pt+ p dir))
        (n1 (pt+ init dir) (pt+ n1 dir))
        (n2 (pt+ n1 dir) (pt+ n2 dir)))
       ((or (char= (pt-ref ary n1) #\*) (char= (pt-ref ary n2) #\*))
        (setf (pt-ref ary p) #\*)
        p)
    (setf (pt-ref ary p) #\*)))

(defun print-test-case (ary size)
  (loop for i from 2 to (1+ (pt-y size)) do
    (loop for j from 2 to (1+ (pt-x size)) do
      (write-char (aref ary i j)))
    (write-char #\Newline)))

; demo output

(defvar *size* (make-pt :x 4 :y 5))
(defvar *ary* (make-test-case-array *size*))
(format t "~A~%" (walk-asap *ary* (make-pt :x 2 :y 2) +r1+))
(print-test-case *ary* *size*)

