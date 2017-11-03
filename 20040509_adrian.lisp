; FCTRL - Factorial
; http://www.spoj.com/problems/FCTRL/

;; !! I DID NOT SOLVED !!

(defconstant *MAX* (expt 10 6))
(defconstant *STEP* (expt 10 3))

(defvar *twos* 0)
(defvar *fives* 0)

(defun take-two (n)
  (let ((cur n))
    (loop
      (unless (zerop (mod cur 2)) (return-from take-two cur))
      (setq cur (/ cur 2))
      (incf *twos*))))

(defun take-five (n)
  (let ((cur n))
    (loop
      (unless (zerop (mod cur 5)) (return-from take-five cur))
      (setf cur (/ cur 5))
      (incf *fives*))))

(defvar *mem-two* (make-array (1+ (ceiling (/ *MAX* *STEP*)))))
(defvar *mem-five* (make-array (1+ (ceiling (/ *MAX* *STEP*)))))
(setf (aref *mem-two* 0) 0)
(setf (aref *mem-five* 0) 0)
(do ((i 1 (1+ i))) ((> i *MAX*))
  (take-five (take-two i))
  (when (zerop (mod i *STEP*))
    (setf (aref *mem-two* (/ i *STEP*)) *twos*)
    (setf (aref *mem-five* (/ i *STEP*)) *fives*)))
;(print *mem-two*)
;(print *mem-five*)

;(do ((i 1 (1+ i))) ((> i 10))
;  (print (take-two i))
;  (print *twos*))

