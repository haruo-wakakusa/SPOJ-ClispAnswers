; SPTTRN3 - Straight Line Spiral Pattern (Act 3)
; http://www.spoj.com/problems/SPTTRN3/

; this is not completed.

(defstruct pt (x 0) (y 0))

(defun pt+ (a b)
  (make-pt :x (+ (pt-x a) (pt-x b)) :y (+ (pt-y a) (pt-y b))))
(defun pt- (a b)
  (make-pt :x (- (pt-x a) (pt-x b)) :y (- (pt-y a) (pt-y b))))
(defun pt= (a b)
  (and (= (pt-x a) (pt-x b)) (= (pt-y a) (pt-y b))))
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
  (list +r1+ +d1+ +l1+ +u1+ +r2+ +d2+ +r3+ +u2+ +l2+ +d3+ +r1+))

(defun get-next-direction (dir)
  (let ((rot +rotation+))
    (block nil (loop
      (when (eq (car rot) dir) (return (cadr rot)))
      (setf rot (cdr rot))))))

; make-debug-array
(defun mdary (row col)
  (let ((res (make-array (list row col))))
    (dotimes (i row)
      (let ((str (read-line *standard-input*)))
        (dotimes (j col)
          (when (< j (length str)) (setf (aref res i j) (char str j))))))
    res))

(defconstant +4-neighbor+
  (list (make-pt :x 1 :y 0) (make-pt :x 0 :y 1)
        (make-pt :x -1 :y 0) (make-pt :x 0 :y -1)))

; contact check for endpoint
(defun in-contact-p (ary pt char)
  (let* ((neighbor (mapcar (lambda (delta) (pt+ pt delta)) +4-neighbor+))
         (val (mapcar (lambda (pt) (pt-ref ary pt)) neighbor))
         (is-same (mapcar (lambda (val) (if (char= val char) 1 0)) val)))
    (> (apply #'+ is-same) 1)))

(defconstant +clockwise+ (make-array 8 :initial-contents (list
  (make-pt :x -1 :y -1) (make-pt :x 0 :y -1)
  (make-pt :x 1 :y -1) (make-pt :x 1 :y 0)
  (make-pt :x 1 :y 1) (make-pt :x 0 :y 1)
  (make-pt :x -1 :y 1) (make-pt :x -1 :y 0))))

(defun connected-near-point-p (ary pt char)
  (let ((cnt) (rise 0))
    (setf cnt (loop for i below 8 sum
      (if (char= (pt-ref ary (pt+ pt (aref +clockwise+ i))) char) 1 0)))
    (when (zerop cnt) (return-from connected-near-point-p nil))
    (when (= cnt 8) (return-from connected-near-point-p t))
    (do ((cur 0 (mod (1+ cur) 8)) (prev 7 (mod (1+ prev) 8)) (i 0 (1+ i)))
        ((= i 8))
      (when
        (and (not (char= (pt-ref ary (pt+ pt (aref +clockwise+ prev))) char))
             (char= (pt-ref ary (pt+ pt (aref +clockwise+ cur))) char))
        (incf rise)))
    (= rise 1)))

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
  (do ((p (pt+ init dir) (pt+ p dir)))
      ((or (in-contact-p ary p #\*)
           (not (connected-near-point-p ary p #\.)))
       (pt- p dir))
    (setf (pt-ref ary p) #\*)))

(defun walk-a-little (ary init dir)
  (do ((p (pt+ init dir) (pt+ p dir)) (i 0 (1+ i)))
      ((or (= i 2)
           (in-contact-p ary p #\*)
           (not (connected-near-point-p ary p #\.)))
       (and (= i 2) (pt- p dir)))
    (setf (pt-ref ary p) #\*)))

(defun walkable-p (ary init dir)
  (let ((next (pt+ init dir)))
    (and (not (in-contact-p ary next #\*))
         (connected-near-point-p ary next #\.))))

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
    (setf (pt-ref ary (make-pt :x 2 :y 2)) #\*)
    (walk ary (make-pt :x 2 :y 2) +r1+)
    (print-test-case ary size)))

(defvar *t* (read))
(dotimes (i *t*)
  (solve-test-case (read))
  (unless (= i (1- *t*)) (write-char #\Newline)))

