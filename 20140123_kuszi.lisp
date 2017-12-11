; SMPCPH1 - Substitution cipher
; http://www.spoj.com/problems/SMPCPH1/

(defvar *n* (read-from-string (read-line)))
(defvar *s* (map 'list #'identity (read-line)))
(setf *s* (cons (car (last *s*)) *s*))

(defun encode (char list)
  (cond ((null (cdr list)) char)
        ((char= char (car list)) (cadr list))
        (t (encode char (cdr list)))))

(defvar *m* (read-from-string (read-line)))

(dotimes (_ *m*)
  (format t "~A~%" (map 'string (lambda (c) (encode c *s*)) (read-line))))

