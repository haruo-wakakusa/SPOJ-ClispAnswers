; CPTTRN2 - Character Patterns (Act 2)
; http://www.spoj.com/problems/CPTTRN2/

(defun framep (row col i j)
  (or (zerop i) (= i (1- row)) (zerop j) (= j (1- col))))

(defun print-pattern (row col)
  (dotimes (i row)
    (dotimes (j col)
      (write-char (if (framep row col i j) #\* #\.)))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read))

