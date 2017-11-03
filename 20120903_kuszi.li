; CPTTRN1 - Character Patterns (Act 1)
; http://www.spoj.com/problems/CPTTRN1/

(defun print-pattern (row col)
  (dotimes (i row)
    (dotimes (j col)
      (write-char (if (evenp (+ i j)) #\* #\.)))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read))

