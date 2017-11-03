; CPTTRN5 - Character Patterns (Act 5)
; http://www.spoj.com/problems/CPTTRN5/

(defun get-even-pattern (i j cell-size) (if (= i j) #\\ #\.))
(defun get-odd-pattern (i j cell-size)
  (if (= i (- (1- cell-size) j)) #\/ #\.))

(defun framep (i j cell-size)
  (or (zerop (mod i (1+ cell-size)))
      (zerop (mod j (1+ cell-size)))))

(defun in-even-patternp (i j cell-size)
  (evenp (+ (floor (/ i (1+ cell-size)))
            (floor (/ j (1+ cell-size))))))

(defun get-row-offset (i cell-size) (1- (mod i (1+ cell-size))))
(defun get-col-offset (j cell-size) (1- (mod j (1+ cell-size))))

(defun print-pattern (row col cell-size)
  (dotimes (i (1+ (* (1+ cell-size) row)))
    (dotimes (j (1+ (* (1+ cell-size) col)))
      (cond
        ((framep i j cell-size) (write-char #\*))
        ((in-even-patternp i j cell-size)
           (write-char (get-even-pattern (get-row-offset i cell-size)
                                         (get-col-offset j cell-size)
                                         cell-size)))
        (t (write-char (get-odd-pattern (get-row-offset i cell-size)
                                        (get-col-offset j cell-size)
                                        cell-size)))))
    (write-char #\Newline)))

(defvar *t* (read))
(dotimes (i (1- *t*))
  (print-pattern (read) (read) (read))
  (write-char #\Newline))
(print-pattern (read) (read) (read))

