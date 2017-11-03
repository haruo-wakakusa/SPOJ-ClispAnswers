; CPTTRN4 - Character Patterns (Act 4)
; http://www.spoj.com/problems/CPTTRN4/

(defun framep (cell-height cell-width i j)
  (or (zerop (mod i (1+ cell-height))) (zerop (mod j (1+ cell-width)))))

(defun print-pattern (row col cell-height cell-width)
  (dotimes (i (1+ (* (1+ cell-height) row)))
    (dotimes (j (1+ (* (1+ cell-width) col)))
      (write-char (if (framep cell-height cell-width i j) #\* #\.)))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read) (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read) (read) (read))

