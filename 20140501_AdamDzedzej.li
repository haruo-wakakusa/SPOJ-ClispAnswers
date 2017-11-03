; PRIME1 - Prime Generator (Adam Dzedzej, 2004-05-01)
; http://www.spoj.com/problems/PRIME1/


(defconstant *max* (expt 10 9))

;;
;; make segments between 1 and sqrt(*max*)
;;

(defun make-sqrt-series (n)
  (if (< n 100) (list n)
                (cons n (make-sqrt-series (ceiling (sqrt n))))))
(defvar segments (reverse (cdr (make-sqrt-series *max*))))

;;
;; solve primes for the smallest segment (<=100)
;;

(defun slow-primep (n)
  (let ((i 2))
  (loop
    (when (>= i n) (return-from slow-primep t))
    (when (zerop (mod n i)) (return-from slow-primep nil))
    (incf i)))
  nil)

(defun make-slow-prime-list (n)
  (let ((res nil))
    (do ((i (1- n) (1- i))) ((< i 2))
      (when (slow-primep i) (setf res (cons i res))))
    res))

(defvar primes (make-slow-prime-list (car segments)))

;;
;; expand primes with respect to segments
;;

(defun with-smaller-primes-primep (n primes)
  (dolist (p primes)
    (when (zerop (mod n p)) (return-from with-smaller-primes-primep nil)))
  t)

(defun make-segment-prime-list (start end primes)
  (let ((res nil))
    (do ((i end (1- i))) ((< i start))
      (when (with-smaller-primes-primep i primes) (setf res (cons i res))))
    res))

(defun expand-primes (segs)
  (unless (cdr segs) (return-from expand-primes))
  (let ((larger (make-segment-prime-list (1+ (car segs)) (cadr segs) primes)))
    (rplacd (last primes) larger))
  (expand-primes (cdr segs)))

(expand-primes segments)

;;
;;  solve problem
;;

(defun check-at-composite-number (prime start end ary)
  (let ((i (+ start (mod (- prime start) prime))))
    (when (= i prime) (setf i (+ i prime)))
    (loop
      (when (> i end) (return-from check-at-composite-number))
      (setf (aref ary (- i start)) t)
      (setf i (+ i prime)))))

(defun solve-test-case (start end)
  (let ((ary (make-array (list (1+ (- end start))))))
    (dolist (p primes) (check-at-composite-number p start end ary))
    (when (= start 1) (setf (aref ary 0) t))
    (dotimes (i (1+ (- end start)))
      (when (not (aref ary i)) (print (+ start i))))))

(defvar t1 (read))
(dotimes (i t1)
  (solve-test-case (read) (read))
  (format *standard-output* "
"))

