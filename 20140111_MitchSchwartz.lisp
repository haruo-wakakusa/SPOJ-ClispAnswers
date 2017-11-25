; PCROSS1 - Cross Pattern (Act 1)
; http://www.spoj.com/problems/PCROSS1/

(defconstant +max-m+ 100)

(defvar *normal-string-template*
  (with-output-to-string (s)
    (dotimes (i (1- +max-m+)) (write-char #\. s))
    (write-char #\* s)
    (dotimes (i (1- +max-m+)) (write-char #\. s))))

(defvar *filled-string-template*
  (with-output-to-string (s)
    (dotimes (i +max-m+) (write-char #\* s))))

(defun get-normal-string (n cj)
  (subseq *normal-string-template*
          (- +max-m+ cj)
          (+ (- +max-m+ cj) n)))

(defun get-filled-string (n)
  (subseq *filled-string-template* 0 n))

(defun write-pattern (m n ci cj)
  (let ((normal (get-normal-string n cj)) (filled (get-filled-string n)))
    (loop for i from 1 to m do
      (format t "~A~%" (if (= i ci) filled normal)))))

(defvar *t* (read))
(dotimes (i *t*)
  (write-pattern (read) (read) (read) (read))
  (unless (= i (1- *t*)) (write-char #\Newline)))

