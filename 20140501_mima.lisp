; TEST - Life, the Universe, and Everything

(defun read-and-print ()
  (let ((num (read)))
    (when (not (= num 42))
          (print num)
          (read-and-print))))

(read-and-print)
(exit)
