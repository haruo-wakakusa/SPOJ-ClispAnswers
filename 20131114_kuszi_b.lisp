; SMPSEQ4 - Fun with Sequences (Act 2)
; http://www.spoj.com/problems/SMPSEQ4/

(defun string-join (separator string-list)
  (let ((s (make-string-output-stream)) (res))
    (write-string (car string-list) s)
    (dolist (str (cdr string-list))
      (write-string separator s)
      (write-string str s))
    (setf res (get-output-stream-string s))
    (close s)
    res))

(defmacro set= (&rest proc-var-form-list)
  (let ((lst))
    (dolist (pvf proc-var-form-list)
      (let ((proc (first pvf)) (var (second pvf)) (form (third pvf)))
        (when (eq proc 'cons) (psetf var form form var))
        (setf lst (cons pvf (cons var lst)))))
    (setf lst (reverse lst))
    `(setf ,@lst)))

(defvar *n* (read))
(defvar *s* (loop for i below *n* collect (read)))
(defvar *m* (read))
(defvar *q* (loop for i below *m* collect (read)))

(defun uniquify (lst)
  (let ((l lst))
    (loop do
      (when (null (cdr l)) (loop-finish))
      (when (= (first l) (second l)) (rplacd l (cddr l)))
      (setf l (cdr l)))))

(defun get-intersection (a b)
  (let ((res nil))
    (block nil
      (let ((rest-a a) (rest-b b) (a1 (car a)) (b1 (car b)))
        (loop
          (when (or (null rest-a) (null rest-b)) (return))
          (cond ((= a1 b1) (set= (cons a1 res) (cdr rest-a)))
                ((< a1 b1) (set= (cdr rest-a)))
                (t         (set= (cdr rest-b))))
          (setf a1 (car rest-a) b1 (car rest-b)))))
    (reverse res)))

(uniquify *q*)
(format t "~A~%"
  (string-join " " (mapcar #'write-to-string (get-intersection *s* *q*))))

