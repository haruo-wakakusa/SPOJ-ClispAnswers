; PCROSS2 - Cross Pattern (Act 2)
; http://www.spoj.com/problems/PCROSS2/

(defun print-test-case (m n ci cj)
  (loop for i from 1 to m do
    (loop for j from 1 to n do
      (if (= (abs (- ci i)) (abs (- cj j))) (write-char #\*)
                                            (write-char #\.)))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i *t*)
  (print-test-case (read) (read) (read) (read))
  (unless (= i (1- *t*)) (write-char #\Newline)))

