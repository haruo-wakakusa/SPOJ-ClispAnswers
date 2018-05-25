; ONP - Transform the Expression
; http://www.spoj.com/problems/ONP/

(defun insert-space (str)
  (with-output-to-string (s)
    (every (lambda (c) (write-char c s) (write-char #\space s) t) str)))

(defun print-symbol (sym)
  (write-char (aref (string-downcase (symbol-name sym)) 0)))

(defun print-forth (tree)
  (if (not (listp tree))
      (print-symbol tree)
      (progn
        (print-forth (first tree))
        (print-forth (third tree))
        (print-symbol (second tree)))))

(defun solve-testcase (str)
  (if (= (length str) 1)
      (write-string str)
      (print-forth (read-from-string (insert-space str)))))

(defvar *n* (read))
(dotimes (i *n*) (solve-testcase (read-line)) (write-char #\newline))

