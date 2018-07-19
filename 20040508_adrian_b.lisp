; BULK - The Bulk!
; https://www.spoj.com/problems/BULK/

;                      [Problem]
;             *-----*  @ A bulk is an assembly of cubes.
;            /     /|  @ The input is informations of the bulk surface(face).
;           /     / |  @ The output is its own volume.
;      *---/     /  *
;     /   *-----*  /   [Note]
;    /    |     | *    @ Bulks can have some empty spaces.
;   *-----*     | |      (ex. a box in a box)
;   |           | *    @ Bulks can be non-path-connected.
;   |           |/       (ex. a "C"-shaped instance)
;   *-----------*
;                      [Strategy]
;         *-----*      @ In view from any orientation, any faces has own
;         |     |        pair. Or, the number of faces are absolutely even,
;   *-----*-----*        not odd number for each unit.
;   |     *-----*      @ We use a grid instead of the exact projection
;   |     |     |        in order to simplify the algorithm.
;   *-----*-----*      @ We treat only faces pallalel to YZ-plane
;                        (We are viewing it from x = -INF).
;   *-----*-----*      @ For each grid segment, we calculate the depth of
;   |     |     |        the bulk due to counting the crossing faces.
;   *-----*-----*
;   *-----*-----*      @ The above 3D-problem reduces a 2D-problem. 
;   |     |     |        "Which is a grid segment in a face, or not?"
;   *-----*-----*      @ We solve it to count the crossing edges pallalel to
;                        Z-axis.

(defmacro pipe (init &rest forms)
"Like F#'s pipeline-operator or Clojure's threading macro. The forms
evaluate in order. The variable $ are the init value or the value of former
expression."
  (do* ((rest forms (cdr rest)) (res init))
       ((null rest) res)
    (setq res `(let (($ ,res)) ,(car rest)))))


(defstruct point (x) (y) (z))
(defstruct z-range (start) (end))

(defun list-shift-right (list)
"Example: '(1 2 3 4) -> '(4 1 2 3)"
  (cons (first (last list)) (butlast list)))

(defun list-shift-left (list)
"Example: '(1 2 3 4) -> '(2 3 4 1)"
  (append (rest list) (list (first list))))

(defun get-orientation (p1 p2)
"For the path from the point p1 to the point p2, returns the orientation
(:x, :y or :z)"
  (let ((dx (- (point-x p2) (point-x p1)))
        (dy (- (point-y p2) (point-y p1)))
        (dz (- (point-z p2) (point-z p1))))
    (cond ((and (not (zerop dx)) (zerop dy) (zerop dz)) :x)
          ((and (zerop dx) (not (zerop dy)) (zerop dz)) :y)
          ((and (zerop dx) (zerop dy) (not (zerop dz))) :z)
          (t (error "error in get-orientation")))))

(defun remove-if-not-edge (face)
  (loop for cur in face
        and left in (list-shift-left face)
        and right in (list-shift-right face)
        unless (eq (get-orientation cur left) (get-orientation cur right))
        collect cur))

(defun pallalel-to-yz-plane-p (face)
  (notany (lambda (p1 p2) (eq (get-orientation p1 p2) :x))
          face
          (list-shift-right face)))

(defun remove-if-not-pallalel-to-yz-plane (faces)
  (remove-if-not #'pallalel-to-yz-plane-p faces))

(defun get-grid-points (faces axis-accessor)
  (pipe faces
        (apply #'append $)
        (mapcar axis-accessor $)
        (remove-duplicates $)
        (sort $ #'<)))

(defun read-testcase ()
  (loop for i below (read) collect
    (loop for j below (read) collect
      (make-point :x (read) :y (read) :z (read)))))

(defun solve-testcase ()
  (let* ((raw-faces (read-testcase))
         (faces (pipe raw-faces
                      (remove-if-not-pallalel-to-yz-plane $)
                      (mapcar #'remove-if-not-edge $)))
         (y-grid-points (get-y-grid-points faces #'point-y))
         (z-grid-points (get-z-grid-points faces #'point-z)))
    z-grid-points))

