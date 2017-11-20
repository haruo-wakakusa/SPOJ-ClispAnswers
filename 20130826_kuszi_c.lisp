; SMPDIV - Divisibility
; http://www.spoj.com/problems/SMPDIV/

(defun solve (n x y)
  (do ((i x (+ i x)) (is-first t)) ((>= i n))
    (unless (zerop (mod i y))
      (if is-first (setf is-first nil) (write-char #\Space))
      (format t "~d" i)))
  (write-char #\Newline))

(defvar *t* (read))
(dotimes (i *t*) (solve (read) (read) (read)))

