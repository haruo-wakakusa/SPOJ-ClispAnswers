; ARITH - Simple Arithmetics
; https://www.spoj.com/problems/ARITH/

(defun divide (str)
  (let ((pos (or (position #\+ str) (position #\- str) (position #\* str))))
    (list (substring str 0 pos)
          (substring str pos (1+ pos))
          (substring str (1+ pos) (length str)))))

(defun format* (str len right)
  (format t "~a~a~a~%"
    (make-string (- len (length str) right) :initial-element #\Space)
    str
    (make-string right :initial-element #\Space)))

(defun print+ (arg1 arg2 op op-str)
  (let* ((res (format nil "~a" (funcall op (read-from-string arg1)
                                           (read-from-string arg2))))
         (+arg2 (concatenate 'string op-str arg2))
         (cols (max (length arg1) (length +arg2) (length res))))
  (format* arg1 cols 0)
  (format* +arg2 cols 0)
  (format t "~a~%" (make-string cols :initial-element #\-))
  (format* res cols 0)))

(defun print* (arg1 arg2)
  (let* ((res (format nil "~a" (* (read-from-string arg1)
                                  (read-from-string arg2)))
