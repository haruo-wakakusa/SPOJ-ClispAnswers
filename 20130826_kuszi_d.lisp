; SMPCIRC - Two Circles
; http://www.spoj.com/problems/SMPCIRC/

(defstruct circle (x 0 :type fixnum) (y 0 :type fixnum) (r 0 :type fixnum))

(defun sqr (x) (* x x))

(defun circle= (c1 c2)
  (and (= (circle-x c1) (circle-x c2))
       (= (circle-y c1) (circle-y c2))
       (= (circle-r c1) (circle-r c2))))

(defun discriminate (small large)
  (let* ((distance-sqr (+ (sqr (- (circle-x large) (circle-x small)))
                          (sqr (- (circle-y large) (circle-y small)))))
         (det (- distance-sqr (sqr (- (circle-r large) (circle-r small))))))
    (cond ((< det 0) 'i)
          ((zerop det) 'e)
          (t 'o))))

(defun read-circle (circle)
  (setf (circle-x circle) (read)
        (circle-y circle) (read)
        (circle-r circle) (read)))

(defvar *t* (read))
(defvar *c1* (make-circle))
(defvar *c2* (make-circle))
(dotimes (i *t*)
  (read-circle *c1*)
  (read-circle *c2*)
  (print (if (< (circle-r *c1*) (circle-r *c2*))
                (discriminate *c1* *c2*)
                (discriminate *c2* *c1*))))

