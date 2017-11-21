; HS12MBR - Minimum Bounding Rectangle
; http://www.spoj.com/problems/HS12MBR/

; initial value is parameterized to be able to expand it
(defstruct rect (left 1001 :type fixnum) (top 1001 :type fixnum)
                (right -1001 :type fixnum) (bottom -1001 :type fixnum))

(defun get-point-rect (x y rect)
  (setf (rect-left rect) x (rect-top rect) y
        (rect-right rect) x (rect-bottom rect) y))

(defun get-circle-rect (x y r rect)
  (setf (rect-left rect) (- x r) (rect-top rect) (- y r)
        (rect-right rect) (+ x r) (rect-bottom rect) (+ y r)))

(defun get-line-rect (x1 y1 x2 y2 rect)
  (setf (rect-left rect) (min x1 x2) (rect-top rect) (min y1 y2)
        (rect-right rect) (max x1 x2) (rect-bottom rect) (max y1 y2)))

(defun expand-rect (dst src)
  (setf (rect-left dst) (min (rect-left dst) (rect-left src))
        (rect-top dst) (min (rect-top dst) (rect-top src))
        (rect-right dst) (max (rect-right dst) (rect-right src))
        (rect-bottom dst) (max (rect-bottom dst) (rect-bottom src))))

(defun read-shape (rect)
  (case (read)
    ('p (get-point-rect (read) (read) rect))
    ('c (get-circle-rect (read) (read) (read) rect))
    ('l (get-line-rect (read) (read) (read) (read) rect))))

(defvar *t* (read))
(defvar *n* 0)
(dotimes (i *t*)
  (let ((n (read)) (res (make-rect)) (shape (make-rect)))
    (dotimes (j n)
      (read-shape shape)
      (expand-rect res shape))
    (format t "~d ~d ~d ~d~%" (rect-left res) (rect-top res)
                              (rect-right res) (rect-bottom res))))

