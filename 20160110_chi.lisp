; CHI_BINARY - Binary numbers
; http://www.spoj.com/problems/CHI_BINARY/

(setf *print-base* 2)
(setf *read-base* 2)

(defun p1 (x) (format t "~b~%" x))
(defun p2 (x y) (format t "~b ~b~%" x y))

(defun sub (a b)
  (if (< a b)
      (let ((2n (do ((2n 1 (* 2n 10))) ((> 2n (max a b)) 2n))))
        (- (+ a 2n) b))
      (- a b)))

(defvar *t* (read))
(dotimes (_ *t*)
  (let ((op (read)) (a (read)) (b (read)))
    (case op
      (0 (p1 (if (> a b) 1 0)))
      (1 (p1 (+ a b)))
      (10 (p1 (sub a b)))
      (11 (p1 (* a b)))
      (100 (p2 (floor (/ a b)) (mod a b))))))

