; SERI07 - Strange But Easy
; http://www.spoj.com/problems/SERI07/

(defconstant +max-integer+ 10000)

(defun small-prime-p (n)
  (do ((i 2 (1+ i))) ((= i n) t)
    (when (zerop (mod n i)) (return nil))))

(defvar *small-primes*
  (loop for i from 2 to 1000 when (small-prime-p i) collect i))

(defun prime-p (n)
  (if (<= n 1000)
      (find n *small-primes*)
      (dolist (p *small-primes* t)
        (when (zerop (mod n p)) (return nil)))))

(defvar cur 1)
(defun get-next-prime ()
  (incf cur)
  (block nil (loop
    (when (prime-p cur) (return))
    (incf cur)))
  cur)

(defvar *primes* (make-array (* 3 +max-integer+)))
(loop for i below (* 3 +max-integer+) do
  (setf (aref *primes* i) (get-next-prime)))

(defvar *delta* (loop for i from 2 below (* 3 +max-integer+)
                      collect (- (aref *primes* i) (aref *primes* (1- i)))))

(dolist (n *delta*)
  (if (< n (* 26 2)) (princ (code-char (+ (char-code #\a) (/ n 2))))
                     (princ (code-char (+ (char-code #\A) (- (/ n 2) 26))))))

