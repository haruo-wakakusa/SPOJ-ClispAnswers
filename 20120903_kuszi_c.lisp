; CPTTRN3 - Character Patterns (Act 3)
; http://www.spoj.com/problems/CPTTRN3/

(defun framep (row col i j)
  (or (zerop (mod i 3)) (zerop (mod j 3))))

(defun print-pattern (row col)
  (dotimes (i (1+ (* 3 row)))
    (dotimes (j (1+ (* 3 col)))
      (write-char (if (framep row col i j) #\* #\.)))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read))

