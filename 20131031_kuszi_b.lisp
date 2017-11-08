; SPTTRN1 - Straight Line Spiral Pattern (Act 1)
; http://www.spoj.com/problems/SPTTRN1/

(defconstant *right* (list 0 1))
(defconstant *down* (list 1 0))
(defconstant *left* (list 0 -1))
(defconstant *up* (list -1 0))

(defun get-next-direction (dir)
  (cond ((eq dir *right*) *down*)
        ((eq dir *down*) *left*)
        ((eq dir *left*) *up*)
        (t *right*)))

(defun make-snake (size ary init-i init-j)
  (let ((i init-i) (j init-j) (dir *right*))
    (flet
      ((walk ()
        (let ((nexti i) (nextj j) (n2i) (n2j))
          (block nil
            (loop
              (setf nexti (+ nexti (first dir)))
              (setf nextj (+ nextj (second dir)))
              (setf n2i (+ nexti (first dir)))
              (setf n2j (+ nextj (second dir)))
              (when (eq (aref ary nexti nextj) #\.) (return))
              (when (eq (aref ary n2i n2j) #\.) (return))
              (setf (aref ary nexti nextj) #\.)))
          (setf nexti (- nexti (first dir)))
          (setf nextj (- nextj (second dir)))
          (when (and (= nexti i) (= nextj j)) (return-from walk :dead))
          (setf i nexti j nextj dir (get-next-direction dir))
          :living)))
      #'walk)))

(defun make-pattern (size)
  (let ((ary (make-array (list (+ size 2) (+ size 2)))))
    (dotimes (i (+ size 2)) (dotimes (j (+ size 2))
      (setf (aref ary i j) #\.)))
    (loop for i from 1 to size do
      (loop for j from 1 to size do (setf (aref ary i j) #\*)))
    ary))

(defun print-pattern (pat size)
  (loop for i from 1 to size do
    (loop for j from 1 to size do (write-char (aref pat i j)))
    (write-char #\Newline)))

(defun print-test-case (size)
  (let ((pat (make-pattern size)))
    (do ((snake (make-snake (+ size 2) pat 2 0)))
        ((eq (funcall snake) :dead))
        nil)
    (print-pattern pat size)))

(defvar *t* (read))
(dotimes (i *t*)
  (print-test-case (read))
  (write-char #\Newline))

