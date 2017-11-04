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

(defun print-pattern (row col size)
  (let* ((aryrow (* row size 2)) (arycol (* col size 2))
         (ary (make-array (list aryrow arycol))))
    (dotimes (i aryrow) (dotimes (j arycol) (setf (aref ary i j) #\.)))
    (write-diamond 0 0 size ary)
    (loop for i from 0 below aryrow by (* 2 size) do
      (loop for j from 0 below arycol by (* 2 size) do
        (write-diamond i j size ary)))
    (dotimes (i aryrow)
      (dotimes (j arycol) (write-char (aref ary i j)))
      (write-char #\Newline))))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read) (read))

