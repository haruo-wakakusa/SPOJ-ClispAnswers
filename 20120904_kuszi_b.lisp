; CPTTRN7 - Character Patterns (Act 7)
; http://www.spoj.com/problems/CPTTRN7/

;
;  !! TIME OVER CODE !!
;

(macrolet (
      (draw (len char i-start i-step j-start j-step)
        `(do ((k 0 (1+ k)) (i ,i-start (+ i ,i-step))
                           (j ,j-start (+ j ,j-step)))
             ((>= k size))
          (setf (aref ary i j) ,char))))
  (defun write-diamond (i-start j-start size ary)
    (draw size #\/ (+ i-start (1- size)) -1 j-start 1)
    (draw size #\\ i-start 1 (+ j-start size) 1)
    (draw size #\/ (+ i-start size) 1 (+ j-start (1- (* 2 size))) -1)
    (draw size #\\ (+ i-start (1- (* 2 size))) -1 (+ j-start (1- size)) -1)
  ))

(defun print-pattern (row col size ary)
  (let* ((aryrow (* size 2)) (arycol (* col size 2)))
    (dotimes (i aryrow) (dotimes (j arycol) (setf (aref ary i j) #\.)))
    (loop for j from 0 below arycol by (* 2 size) do
      (write-diamond 0 j size ary))
    (dotimes (i row)
      (dotimes (j aryrow)
        (dotimes (k arycol) (write-char (aref ary j k)))
          (write-char #\Newline)))))

(defvar *t* (read))
(defvar *tests* (loop for i below *t* collect (list (read) (read) (read))))
(defvar *maxrowsize* 0)
(defvar *maxcolsize* 0)
(dolist (test *tests*)
  (setf *maxrowsize* (max *maxrowsize* (* (third test) 2)))
  (setf *maxcolsize* (max *maxcolsize* (* (second test) (third test) 2))))
(defvar *ary* (make-array (list *maxrowsize* *maxcolsize*)))

(dotimes (i (1- *t*))
  (let ((test (nth i *tests*)))
    (print-pattern (first test) (second test) (third test) *ary*))
  (write-char #\Newline))
(let ((test (nth (1- *t*) *tests*)))
  (print-pattern (first test) (second test) (third test) *ary*))

