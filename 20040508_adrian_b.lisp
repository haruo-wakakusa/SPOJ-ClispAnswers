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


(defun read-testcase ()
  (loop for nil below (read) collect
    (loop for nil below (read) collect (list (read) (read) (read)))))


(defun get-x-y-and-z-ranges (plane)
  (remove-if #'null
    (mapcar
      (lambda (p1 p2)
        (if (or (not (= (first p1) (first p2)))
                (not (= (second p1) (second p2))))
            nil
            (list (first p1)
                  (second p1)
                  (min (third p1) (third p2))
                  (max (third p1) (third p2)))))
      plane
      (cons (first (last plane)) plane))))

(defun yz-grid-in-plane-p (grid plane)
  (let ((count 0))
    (dolist (range (get-x-y-and-z-ranges plane))
      (when (and (<= (second range) (first grid))
                 (<= (third range) (second grid))
                 (> (fourth range) (second grid)))
        (incf count)))
    (oddp count)))

(defun get-planes-including-yz-grid-in (planes//yz-plane grid)
  (remove-if-not (lambda (plane) (yz-grid-in-plane-p grid plane))
                 planes//yz-plane))

(defun get-x-depth-for-yz-grid (planes//yz-plane grid)
  (let ((planes (get-planes-including-yz-grid-in planes//yz-plane grid)))
    (unless (evenp (length planes))
      (error "error in get-x-depth-for-yz-grid"))
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

    (fun points->ranges (points)
      (mapcar #'list points (rest points)))

    (format t "~%MAKING Y-GRID...~%")
    (var y-grid
      (time
      (-> points
          (mapcar #'second $)
          (remove-duplicates $)
          (sort $ #'<)
          (points->ranges $))))
    ;(format t "y-grid = ~a~%" y-grid)

    (format t "~%MAKING Z-GRID...~%")
    (var z-grid
      (time
      (-> points
          (mapcar #'third $)
          (remove-duplicates $)
          (sort $ #'<)
          (points->ranges $))))
    ;(format t "z-grid = ~a~%" z-grid)

    (format t "~%CALCULATING VOLUME...~%")
    (var volume
      (time
      (loop for y-range in y-grid sum
        (* (- (second y-range) (first y-range))
           (loop for z-range in z-grid sum
             (* (- (second z-range) (first z-range))
                (get-x-depth-for-yz-grid planes//yz-plane
                                         (list (first y-range)
                                               (first z-range)))))))))

    volume


  ) ; end of block*
) ; end of defun<solve-testcase>


(defvar *t* (read))
(dotimes (i *t*)
  (format t "The bulk is composed of ~a units.~%"
    (solve-testcase (read-testcase))))

