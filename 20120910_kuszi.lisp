; HS12HDPW - Hidden Password
; http://www.spoj.com/problems/HS12HDPW/

(defun get-index-a (tuple)
  (let ((res 0))
    (dotimes (i 6)
      (setf res (+ res (logand (expt 2 i)
                               (char-code (aref tuple i))))))
    res))

(defun get-index-b (tuple)
  (let ((res 0))
    (dotimes (i 6)
      (unless (zerop (logand (expt 2 (mod (+ i 3) 6))
                             (char-code (aref tuple i))))
        (setf res (+ res (expt 2 i)))))
    res))

(defun get-tuples (n str)
  (do ((i 0 (1+ i)) (start 0 (+ start 7)) (end 6 (+ end 7)) (res nil))
      ((>= i n) (reverse res))
    (setf res (cons (subseq str start end) res))))

(defun solve-test-case (n tuples code)
  (dolist (tuple (get-tuples n tuples))
    (write-char (aref code (get-index-a tuple)))
    (write-char (aref code (get-index-b tuple))))
  (write-char #\Newline))

(defvar *t* (read-from-string (read-line)))
(dotimes (i *t*)
  (solve-test-case (read-from-string (read-line)) (read-line) (read-line))
  (unless (= i (1- *t*)) (read-line)))

