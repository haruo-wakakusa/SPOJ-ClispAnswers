; CHITEST1 - Sum of two numbers
; http://www.spoj.com/problems/CHITEST1/

(defvar *t* (read))
(dotimes (_ *t*) (format t "~d~%" (+ (read) (read))))

