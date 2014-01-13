;; some generic definitions of geometric objects and functions
;; that do stuff to them

(in-package :geom)	     

(defgeneric distance (obj1 obj2)
  (:documentation "The distance between two geometric objects, however defined."))

(defgeneric scale (obj scale-factor)
  (:documentation "Scales a geometric object by a scale factor."))

(defgeneric difference (obj1 obj2)
  (:documentation "Difference between two geometric objects."))

(defgeneric is-geom-valid? (obj)
  (:documentation "Checks the geometric object for validity."))

(defgeneric geom= (obj1 obj2 &key tolerance)
  (:documentation "Are the two objects geometrically equal?"))

(defmethod geom= ((obj1 null) (obj2 geometric-object) &key tolerance)
  (declare (ignore tolerance))
  nil)

(defmethod geom= ((obj1 geometric-object) (obj2 null) &key tolerance)
  (declare (ignore tolerance))
  nil)

(defmethod rad->deg ((rad number))
  (* rad (/ 180.0 pi)))

(defmethod deg->rad ((deg number))
  (* deg (/ pi 180.0)))

(defmethod is-geom-valid? ((obj-list list))
  (loop
     with geom-flags = nil
     for item in obj-list
     do (push (is-geom-valid? item) geom-flags)
     finally (return (every #'identity geom-flags))))
	

(defgeneric rotate-point (point-to-rotate theta ref-point))

;; (defmethod rotate-point ((point-to-rotate point) (theta number) (ref-point point))
;;   "Rotate a point in 2d around a reference point."
;;   (unless ref-point
;;     (setf ref-point (make-point 0 0 0)))
;;   (when (and (is-geom-valid? point-to-rotate)
;; 	     (is-geom-valid? ref-point)))
;;     (let ((px
;; 	   (-
;; 	    (* (cos theta) (- (point-x point-to-rotate) (point-x ref-point)))
;; 	    (* (sin theta) (- (point-y point-to-rotate) (point-y ref-point)))
;; 	    (- (point-x ref-point))))
;; 	  (py
;; 	   (+
;; 	    (* (sin theta) (- (point-x point-to-rotate) (point-x ref-point)))
;; 	    (* (cos theta) (- (point-y point-to-rotate) (point-y ref-point)))
;; 	    (point-y ref-point))))
;;       (make-point px py 0)))

(defun >< (x lim1 lim2)
  (when (or (and (> x lim1)
		 (< x lim2))
    (and (< x lim1)
		 (> x lim2)))
    t))

(defmethod all-distances ((p point) (ref-points list))
  "Compute all the distances between the target point and the reference points."
  (loop
     for ref-point in ref-points
     collect
       (list p ref-point (distance-2d p ref-point))))

(defmethod max-distance ((p point) (ref-points list))
  "Compute the maximum of all the distances between the target point and the
reference points."
  (third
   (first
    (sort (all-distances p ref-points) #'> :key #'third))))

(defmethod min-distance ((p point) (ref-points list))
  "Compute the minimum of all the distances between the target point and the
reference points."
  (third 
   (first
    (sort (all-distances p ref-points) #'< :key #'third))))

(defun average (list-of-things)
  (with-numbers (list-of-things)
    (/ (apply #'+ list-of-things) (length list-of-things))))

(defmethod rotate-objects ((list-of-objects list) (angle number) (ref-point point))
  (assert (every #'is-geom-valid? list-of-objects))
  (loop
     for obj in list-of-objects
     do
       (rotate-object obj angle ref-point)))

(defmethod reflect-point-across-line ((p point) (ls line-segment))
  (let* ((sp (start-point ls))
	 (ep (end-point ls))
	 (m (/ (- (point-y ep) (point-y sp))
	       (- (point-x ep) (point-x sp))))
	 (b (- (point-y sp) (* m (point-x sp))))
	 (d (/ 
	     (+ (point-x p) (* (- (point-y p) b) m))
	     (+ 1 (expt m 2))))
	 (xp (- (* 2 d) (point-x p)))
	 (yp (+ (- (* 2 d m) (point-y p)) (* 2 b))))
    (make-point xp yp)))

(defmethod reflect-objects ((list-of-objects list) (ref-line line-segment))
  (assert (every #'is-geom-valid? list-of-objects))
  (loop
     for obj in list-of-objects
     do
       (reflect-object obj ref-line)))

(defmethod reflect-object ((p point) (ls line-segment))
  (let* ((sp (start-point ls))
	 (ep (end-point ls))
	 (m (/ (- (point-y ep) (point-y sp))
	       (- (point-x ep) (point-x sp))))
	 (b (- (point-y sp) (* m (point-x sp))))
	 (d (/ 
	     (+ (point-x p) (* (- (point-y p) b) m))
	     (+ 1 (expt m 2))))
	 (xp (- (* 2 d) (point-x p)))
	 (yp (+ (- (* 2 d m) (point-y p)) (* 2 b))))
    (set-coords p `(,(cons :x xp) ,(cons :y yp)))))
