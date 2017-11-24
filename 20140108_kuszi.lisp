; BSCXOR - XOR
; http://www.spoj.com/problems/BSCXOR/

(defvar *p* (= (read) 1))
(defvar *q* (= (read) 1))
(defvar *res* (xor *p* *q*))
(format t "~d~%" (if *res* 1 0))

