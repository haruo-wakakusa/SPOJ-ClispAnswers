; CPTTRN8 - Character Patterns (Act 8)
; http://www.spoj.com/problems/CPTTRN8/

; thanks to Doug Hoyte.

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                        (car x)
                        (rec (cdr x) acc))))))
    (rec x nil)))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
               syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;
; main code
;

(defmacro! fill-dot (o!ary o!size)
  `(dotimes (,g!i ,g!size) (dotimes (,g!j ,g!size)
    (setf (aref ,g!ary ,g!i ,g!j) #\.))))

(defmacro! draw-line (o!ary o!char o!len o!i-start o!i-step o!j-start o!j-step)
  `(do ((,g!k 0 (1+ ,g!k))
        (,g!i ,g!i-start (+ ,g!i ,g!i-step))
        (,g!j ,g!j-start (+ ,g!j ,g!j-step)))
       ((>= ,g!k ,g!len))
    (setf (aref ,g!ary ,g!i ,g!j) ,g!char)))

(defmacro! in-diamond-p (o!i o!j o!size)
  `(and (>= ,g!i (- ,g!size ,g!j))
        (> ,g!i (- ,g!j ,g!size))
        (< ,g!i (+ ,g!j ,g!size))
        (< ,g!i (- (* 3 ,g!size) 1 ,g!j))))

(defmacro! fill-diamond (o!ary o!size)
  `(let ((,g!twice (* 2 ,g!size)))
    (dotimes (,g!i ,g!twice) (dotimes (,g!j ,g!twice)
      (when (in-diamond-p ,g!i ,g!j ,g!size)
        (setf (aref ,g!ary ,g!i ,g!j) #\*))))))

(defmacro! make-pattern-array (o!size)
  `(let* ((,g!twice (* 2 ,g!size))
          (,g!ary (make-array (list ,g!twice ,g!twice))))
    (fill-dot ,g!ary ,g!twice)
    (draw-line ,g!ary #\/ ,g!size (1- ,g!size) -1 0 1)
    (draw-line ,g!ary #\\ ,g!size 0 1 ,g!size 1)
    (draw-line ,g!ary #\/ ,g!size ,g!size 1 (1- ,g!twice) -1)
    (draw-line ,g!ary #\\ ,g!size (1- ,g!twice) -1 (1- ,g!size) -1)
    (fill-diamond ,g!ary ,g!size)
    ,g!ary))

(defmacro! print-test-case (o!row o!col o!size)
  `(let ((,g!ary (make-pattern-array ,g!size)) (,g!twice (* 2 ,g!size)))
    (dotimes (,g!i ,g!row)
      (dotimes (,g!j ,g!twice)
        (dotimes (,g!k ,g!col)
          (dotimes (,g!l ,g!twice)
            (write-char (aref ,g!ary ,g!j ,g!l))))
        (write-char #\Newline)))))

(defvar *t* (read))
(dotimes (i *t*)
  (print-test-case (read) (read) (read))
  (unless (= i (1- *t*)) (write-char #\Newline)))

