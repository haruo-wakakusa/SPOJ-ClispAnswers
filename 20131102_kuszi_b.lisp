; SPTTRN3 - Straight Line Spiral Pattern (Act 3)
; http://www.spoj.com/problems/SPTTRN3/

; !! this is not completed !!

(defstruct pt (x 0) (y 0))

(defun pt+ (a b)
  (make-pt :x (+ (pt-x a) (pt-x b)) :y (+ (pt-y a) (pt-y b))))
(defmacro pt-ref (ary pt) `(aref ,ary (pt-y ,pt) (pt-x ,pt)))

(defconstant +r1+ (make-pt :x 1 :y 0)) ; right
(defconstant +r2+ (make-pt :x 1 :y 0))
(defconstant +r3+ (make-pt :x 1 :y 0))
(defconstant +r4+ (make-pt :x 1 :y 0))
(defconstant +d1+ (make-pt :x 0 :y 1)) ; down
(defconstant +d2+ (make-pt :x 0 :y 1))
(defconstant +d3+ (make-pt :x 0 :y 1))
(defconstant +d4+ (make-pt :x 0 :y 1))
(defconstant +l1+ (make-pt :x -1 :y 0)) ; left
(defconstant +l2+ (make-pt :x -1 :y 0))
(defconstant +l3+ (make-pt :x -1 :y 0))
(defconstant +u1+ (make-pt :x 0 :y -1)) ; up
(defconstant +u2+ (make-pt :x 0 :y -1))
(defconstant +u3+ (make-pt :x 0 :y -1))

(defconstant +rotation+
  (list +r1+ +d1+ +l1+ +u1+ +r2+ +d2+ +r3+ +u2+ +l2+
        +d3+ +r4+ +d4+ +l3+ +u3+ +r1+))

(defun get-next-direction (dir)
  (let ((rot +rotation+))
    (block nil (loop
      (when (eq (car rot) dir) (return (cadr rot)))
      (setf rot (cdr rot))))))

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
        (n2 (pt+ n1 dir) (pt+ n2 dir))
        (cnt 0 (1+ cnt)))
       ((or (char= (pt-ref ary n1) #\*) (char= (pt-ref ary n2) #\*))
        (setf (pt-ref ary p) #\*)
        (if (>= cnt 2) p nil))
    (setf (pt-ref ary p) #\*)))

(defun walk-a-little (ary init dir)
  (let* ((n1 (pt+ init dir)) (n2 (pt+ n1 dir)) (n3 (pt+ n2 dir)))
    (when (or (char= (pt-ref ary n1) #\*)
              (char= (pt-ref ary n2) #\*))
      (return-from walk-a-little nil))
    (setf (pt-ref ary n1) #\*)
    (when (char= (pt-ref ary n3) #\*) (return-from walk-a-little nil))
    (setf (pt-ref ary n2) #\*)
    n2))

(defun walkable-p (ary init dir)
  (let* ((n1 (pt+ init dir)) (n2 (pt+ n1 dir)))
    (not (or (char= (pt-ref ary n1) #\*) (char= (pt-ref ary n2) #\*)))))

(defun walk (ary init dir)
  (let ((p (copy-pt init)) (d dir))
    (block nil (loop
      (unless (walkable-p ary p d) (return))
      (setf p (if (or (eq d +r2+) (eq d +d3+))
                  (walk-a-little ary p d)
                  (walk-asap ary p d)))
      (when (null p) (return))
      (setf d (get-next-direction d))))))

(defun print-test-case (ary size)
  (loop for i from 2 to (1+ (pt-y size)) do
    (loop for j from 2 to (1+ (pt-x size)) do
      (write-char (aref ary i j)))
    (write-char #\Newline)))

(defun solve-test-case (n)
  (let* ((size (make-pt :x n :y n))
         (ary (make-test-case-array size)))
    (walk ary (make-pt :x 2 :y 2) +r1+)
    (print-test-case ary size)))

(defvar *t* (read))
(dotimes (i *t*)
  (solve-test-case (read))
  (unless (= i (1- *t*)) (write-char #\Newline)))

