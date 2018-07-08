; ARITH - Simple Arithmetics
; https://www.spoj.com/problems/ARITH/

; this code has bug.

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
  (let* ((res (format nil "~a" (funcall op (parse-integer arg1)
                                           (parse-integer arg2))))
         (+arg2 (concatenate 'string op-str arg2))
         (cols (max (length arg1) (length +arg2) (length res))))
  (format* arg1 cols 0)
  (format* +arg2 cols 0)
  (format t "~a~%" (make-string cols :initial-element #\-))
  (format* res cols 0)))

(defmacro -> (init &rest forms)
  (let ((forms! (loop for form in forms collect `(setq $ ,form))))
    `(let (($ ,init))
      ,@forms!
      $)))

(defun get-mult-series (arg1 arg2 n1 n2)
  (-> arg2
      (reverse $)
      (map 'list #'string $)
      (mapcar #'parse-integer $)
      (mapcar (lambda (n) (* n1 n)) $)
      (mapcar (lambda (n) (format nil "~a" n)) $)))

(defun shift-left (str-list)
  (do* ((lst str-list (rest lst)) (i 0 (1+ i)) (res nil))
       ((null lst) (reverse res))
    (setq res (cons (format nil "~a~a"
                            (first lst)
                            (make-string i :initial-element #\Space))
                    res))))

(defun print* (arg1 arg2)
  (let* ((n1 (parse-integer arg1))
         (n2 (parse-integer arg2))
         (*arg2 (format nil "*~a" arg2))
         (temporary-exps (shift-left (get-mult-series arg1 arg2 n1 n2)))
         (res (format nil "~a" (* n1 n2)))
         (tmp-max (-> temporary-exps
                      (mapcar #'length $)
                      (apply #'max $)))
         (max (max (length arg1) (length *arg2) tmp-max (length res))))
    (format* arg1 max 0)
    (format* *arg2 max 0)
    (format* (make-string (max (length arg1) (length *arg2))
                          :initial-element #\-)
             max 0)
    (dolist (exp temporary-exps) (format* exp max 0))
    (unless (= (length temporary-exps) 1)
      (format t "~a~%" (make-string max :initial-element #\-))
      (format* res max 0))))

(defvar *t* (parse-integer (read-line)))
(dotimes (i *t*)
  (let ((exp (divide (read-line))))
    (cond
      ((string= (second exp) "+") (print+ (first exp) (third exp) #'+ "+"))
      ((string= (second exp) "-") (print+ (first exp) (third exp) #'- "-"))
      ((string= (second exp) "*") (print* (first exp) (third exp)))
      (t (error ""))))
  (write-char #\Newline))

