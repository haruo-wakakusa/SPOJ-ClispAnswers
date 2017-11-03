; CPTTRN6 - Character Patterns (Act 6)
; http://www.spoj.com/problems/CPTTRN6/


(defmacro do-range (param-forms &rest body)
  (let ((var (first param-forms)) (start 0) (end 0) (step 1)
        (len (length param-forms)) (step-val (gensym "STEP-")))
    (cond ((= len 2) (setf end (eval (second param-forms))))
          ((= len 3) (setf start (second param-forms)
                           end (third param-forms)))
          ((= len 4) (setf start (second param-forms)
                           end (third param-forms)
                           step (fourth param-forms))))
    `(let ((,step-val ,step))
      (cond
        ((> ,step-val 0)
          (do ((,var ,start (+ ,var ,step-val))) ((>= ,var ,end)) ,@body))
        ((< ,step-val 0)
          (do ((,var ,start (+ ,var ,step-val))) ((<= ,var ,end)) ,@body))))))


(defun print-pattern (hyphens pipes height width)
  (let* ((rows (1- (* (1+ hyphens) (1+ height))))
         (cols (1- (* (1+ pipes) (1+ width))))
         (ary (make-array (list rows cols))))
    (dotimes (i rows) (dotimes (j cols) (setf (aref ary i j) #\.)))
    (do-range (i height rows (1+ height))
      (dotimes (j cols) (setf (aref ary i j) #\-)))
    (dotimes (i rows)
      (do-range (j width cols (1+ width)) (setf (aref ary i j) #\|)))
    (do-range (i height rows (1+ height))
      (do-range (j width cols (1+ width)) (setf (aref ary i j) #\+)))
    (dotimes (i rows)
      (dotimes (j cols) (write-char (aref ary i j)))
      (write-char #\Newline))))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read) (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read) (read) (read))

