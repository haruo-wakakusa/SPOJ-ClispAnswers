; SMPSEQ7 - Fun with Sequences (Act 5)
; http://www.spoj.com/problems/SMPSEQ7/

(defun solve (lst)
  (labels (
      (dec (lst) (if (or (null (cdr lst)) (<= (first lst) (second lst)))
                     lst
                     (dec (cdr lst))))
      (inc (lst) (if (or (null (cdr lst)) (>= (first lst) (second lst)))
                     lst
                     (inc (cdr lst)))))
    (assert (and (not (null lst)) (not (null (cdr lst)))))
    (let ((dec-end (dec lst)))
      (when (<= (length dec-end) 2) (return-from solve t))
      (<= (length (inc (cdr dec-end))) 1))))

(defvar *n* (read))
(defvar *s* (loop for i below *n* collect (read)))

(format t "~A~%" (if (solve *s*) "Yes" "No"))

