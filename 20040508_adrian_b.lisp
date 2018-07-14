; BULK - The Bulk!
; https://www.spoj.com/problems/BULK/

(defmacro block* (&rest forms)
  (let ((vars nil) (funs nil))
    (dolist (form forms)
      (cond
        ((null form))
        ((atom form))
        ((eq (first form) 'var)
         (do ((rest (rest form) (cddr rest)))
             ((null rest))
           (setq vars (cons (first rest) vars))))
        ((eq (first form) 'fun)
         (setq funs (cons (rest form) funs)))))
    (let ((forms (remove-if (lambda (form) (and (not (atom form))
                                                (eq (first form) 'fun)))
                            forms)))
      (let ((forms (mapcar (lambda (form) (cond ((atom form) form)
                                                ((eq (first form) 'var)
                                                 `(setq ,@(rest form)))
                                                (t form)))
                           forms)))
        `((lambda ()
           (let (,@(mapcar #'list (reverse vars)))
             (labels (,@funs)
               ,@forms))))))))


(defmacro -> (init &rest forms)
  (let ((forms! (loop for form in forms collect `(setq $ ,form))))
    `(let (($ ,init))
      ,@forms!
      $)))


(defstruct y-segment (start 0 :type fixnum) (end 0 :type fixnum))
(defstruct z-segment (start 0 :type fixnum) (end 0 :type fixnum))
(defstruct segment//z-axis (x 0 :type fixnum) (y 0 :type fixnum)
                           (z-start 0 :type fixnum)
                           (z-end 0 :type fixnum))

(defun read-testcase ()
  (loop for nil below (read) collect
    (loop for nil below (read) collect (list (read) (read) (read)))))


(defun get-segment (plane)
  (remove-if #'null
    (mapcar
      (lambda (p1 p2)
        (if (or (not (= (first p1) (first p2)))
                (not (= (second p1) (second p2))))
            nil
            (make-segment//z-axis :x (first p1)
                                  :y (second p1)
                                  :z-start (min (third p1) (third p2))
                                  :z-end (max (third p1) (third p2)))))
      plane
      (cons (first (last plane)) plane))))

(defun yz-segment-in-plane-p (y-segment z-segment plane)
  (let ((count 0))
    (dolist (segment (get-segment plane))
      (when (and (<= (segment//z-axis-y segment) (y-segment-start y-segment))
                 (<= (segment//z-axis-z-start segment)
                     (z-segment-start z-segment))
                 (> (segment//z-axis-z-end segment)
                    (z-segment-start z-segment)))
        (incf count)))
    (oddp count)))

(defun get-planes-including-yz-segment-in (planes//yz-plane
                                           y-segment
                                           z-segment)
  (remove-if-not (lambda (plane) (yz-segment-in-plane-p y-segment
                                                        z-segment
                                                        plane))
                 planes//yz-plane))

(defun get-x-depth-for-yz-segment (planes//yz-plane y-segment z-segment)
  (let ((planes (get-planes-including-yz-segment-in planes//yz-plane
                                                    y-segment
                                                    z-segment)))
    (unless (evenp (length planes))
      (error "error in get-x-depth-for-yz-segment"))
    (setq planes (sort planes (lambda (p1 p2) (< (caar p1) (caar p2)))))
    (do* ((rest planes (cddr rest)) (res 0))
         ((null rest) res)
      (incf res (- (caar (second rest)) (caar (first rest)))))))


(defun solve-testcase (planes)
  (block*

    (format t "~%MAKING POINTS...~%")
    (var points
      (time (apply #'append planes)))
    ;(format t "points = ~a~%" points)

    (fun is-x-constant (plane)
      (every (lambda (p1 p2) (= (first p1) (first p2))) plane (rest plane)))

    (format t "~%MAKING PLANES//YZ-PLANE...~%")
    (var planes//yz-plane
      (time (remove-if-not #'is-x-constant planes)))
    ;(format t "planes//yz-plane = ~a~%" planes//yz-plane)

    (format t "~%MAKING Y-GRID...~%")
    (var y-grid
      (time
      (-> points
          (mapcar #'second $)
          (remove-duplicates $)
          (sort $ #'<)
          (mapcar (lambda (start end) (make-y-segment :start start :end end))
                  $
                  (rest $)))))
    ;(format t "y-grid = ~a~%" y-grid)

    (format t "~%MAKING Z-GRID...~%")
    (var z-grid
      (time
      (-> points
          (mapcar #'third $)
          (remove-duplicates $)
          (sort $ #'<)
          (mapcar (lambda (start end) (make-z-segment :start start :end end))
                  $
                  (rest $)))))
    ;(format t "z-grid = ~a~%" z-grid)

    (format t "~%CALCULATING VOLUME...~%")
    (var volume
      (time
      (loop for y-segment in y-grid sum
        (* (- (y-segment-end y-segment) (y-segment-start y-segment))
           (loop for z-segment in z-grid sum
             (* (- (z-segment-end z-segment)
                   (z-segment-start z-segment))
                (get-x-depth-for-yz-segment planes//yz-plane
                                            y-segment
                                            z-segment)))))))

    volume


  ) ; end of block*
) ; end of defun<solve-testcase>


(defvar *t* (read))
(dotimes (i *t*)
  (format t "The bulk is composed of ~a units.~%"
    (solve-testcase (read-testcase))))

