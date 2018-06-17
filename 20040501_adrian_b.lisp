; PALIN - The Next Palindrome
; https://www.spoj.com/problems/PALIN/

(defun find-not-nine-from-tail (str)
  (do ((i (1- (length str)) (1- i)))
      ((or (= i -1) (not (char= (aref str i) #\9))) i)))

(defun string-increment (str)
  (let* ((i (find-not-nine-from-tail str))
         (upper (subseq str 0 i))
         (middle (subseq str i (1+ i)))
         (lower (subseq str (1+ i) (length str))))
    (format nil "~a~d~a" upper
                         (1+ (read-from-string middle))
                         (make-string (length lower) :initial-element #\0))))

(defun get-next-palindrome (str)
  (let* ((dig (length str))
         (floor (floor (/ dig 2)))
         (ceiling (ceiling (/ dig 2)))
         (a (subseq str 0 floor))
         (b (subseq str floor ceiling))
         (ab (subseq str 0 ceiling))
         (c (subseq str ceiling dig)))
    (cond ((string< c (reverse a)) (concatenate 'string a b (reverse a)))
          ((string= ab (make-string (length ab) :initial-element #\9))
           (concatenate 'string "1"
                                (make-string (1- dig) :initial-element #\0)
                                "1"))
          (t (let* ((ab+1 (string-increment ab)))
               (concatenate 'string ab+1
                                    (reverse (subseq ab+1 0 (length a)))))))))

(defvar *t* (read-from-string (read-line)))
(dotimes (_ *t*) (format t "~a~%" (get-next-palindrome (read-line))))

