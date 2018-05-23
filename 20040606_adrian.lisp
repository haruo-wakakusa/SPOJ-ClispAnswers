; ADDREV - Adding Reversed Numbers
; http://www.spoj.com/problems/ADDREV/

(defun get-reversed-number (n)
  (read-from-string (reverse (format nil "~a" n))))

(defvar *n* (read))
(dotimes (i *n*) (format t "~d~%" (get-reversed-number
                                    (+ (get-reversed-number (read))
                                       (get-reversed-number (read))))))

