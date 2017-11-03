; STRHH - Half of the half
; http://www.spoj.com/problems/STRHH/

(defvar t1 (read))
(dotimes (i t1)
  (let ((str (string-trim '(#\Space #\Newline) (read-line))))
    (do ((j 0 (+ j 2))) ((>= j (/ (length str) 2)))
      (write-char (char str j) *standard-output*))
    (write-char #\Newline *standard-output*)))

