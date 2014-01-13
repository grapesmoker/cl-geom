(in-package :geom)

(defclass point (geometric-object)
  ((x :accessor point-x
      :initform nil
      :initarg :x)
   (y :accessor point-y
      :initform nil
      :initarg :y)
   (z :accessor point-z
      :initform nil
      :initarg :z)))

(defun make-point (&optional x y z)
  "A quick-and-dirty constructor for point"
  (make-instance 'point 
		 :x (if (numberp x) x nil)
		 :y (if (numberp y) y nil)
		 :z (if (numberp z) z nil)))

(defmethod foo ((p1 point) (p2 point))
  (let ((points (list p1 p2)))
    (with-valid-geometry (points)
      (print 'foo))))

(defmethod distance ((p1 point) (p2 point))
  "Euclidean distance between two points."
  (with-valid-geometry ((list p1 p2))
    (sqrt
     (+
      (expt (-
	     (point-x p1)
	     (point-x p2))
	    2)
      (expt (-
	     (point-y p1)
	     (point-y p2))
	    2)
      (expt (-
	     (point-z p1)
	     (point-z p2))
	    2)))))

(defmethod scale ((p point) (k number))
  "Scale a point by a scalar."
  (with-valid-geometry (p)
    (make-instance 'point 
		   :x (* k (point-x p))
		   :y (* k (point-y p))
		   :z (* k (point-z p)))))

(defmethod difference ((p1 point) (p2 point))
  "The `difference' between two points."
  (with-valid-geometry ((list p1 p2))
    (make-point
     (- (point-x p1) (point-x p2))
     (- (point-y p1) (point-y p2))
     (- (point-z p1) (point-z p2)))))
  
(defmethod set-coords ((p point) (coords list))
  "Destructively set the coordinates from a list."
  (setf (point-x p) (cdr (assoc :x coords)))
  (setf (point-y p) (cdr (assoc :y coords)))
  (setf (point-z p) (cdr (assoc :z coords))))

(defmethod set-coords ((p point) (set-to point))
  "Set the coordinates from another point."
  (setf (point-x p) (point-x set-to))
  (setf (point-y p) (point-y set-to))
  (setf (point-z p) (point-z set-to)))
     
(defmethod point->alist ((p point))
  (list
   (cons :x (point-x p))
   (cons :y (point-y p))
   (cons :z (point-z p))))

(defun alist->point (coord-list)
  (make-instance 'point
		 :x (cdr (assoc :x coord-list))
		 :y (cdr (assoc :y coord-list))
		 :z (cdr (assoc :z coord-list))))

(defmethod is-geom-valid? ((p point))
  "Check the point for geometric validity."
  (if (and (numberp (point-x p))
	   (numberp (point-y p))
	   (numberp (point-z p)))
      t
      nil))

(defmethod geometry->json ((p point))
  (format nil "{ x: ~F, y: ~F, z: ~F}" (point-x p) (point-y p) (point-z p)))

(defmethod print-object ((p point) stream)
  (format stream "<#POINT (~3,6F, ~3,6F, ~3,6F)>" (point-x p) (point-y p) (point-z p)))

(defmethod geom= ((p1 point) (p2 point) &key (tolerance 0.01))
  "Are the two points equal to within some tolerance?"
  (with-valid-geometry ((list p1 p2))
    (if (and (< (abs (- (point-x p1) (point-x p2))) tolerance)
	     (< (abs (- (point-y p1) (point-y p2))) tolerance)
	     (or (not 3d) 
		 (< (abs (- (point-z p1) (point-z p2))) tolerance)))
	t
	nil)))

(defmethod copy-geometry ((p point))
  (make-point (point-x p)
	      (point-y p)
	      (point-z p)))

(defun point-centroid (list-of-points)
  (with-valid-geometry (list-of-points)
    (let* ((list-of-coords
	    (loop
	       for p in list-of-points
	       collect (list (point-x p) (point-y p) (point-z p))))
	   (xs (mapcar #'first list-of-coords))
	   (ys (mapcar #'second list-of-coords))
	   (zs (mapcar #'third list-of-coords))
	   (avg-x (average xs))
	   (avg-y (average ys))
	   (avg-z (average zs)))
      (make-point avg-x avg-y avg-z))))

(defmethod rotate-object ((point-to-rotate point) (theta number) (ref-point point))
  "Rotate a point in 2d around a reference point. This function differs from the 
rotate-point-2d function in that it operates in-place, and so does not return anything."
  (unless ref-point
    (setf ref-point (make-point 0 0 0)))
  (when (and (is-geom-valid? point-to-rotate)
	     (is-geom-valid? ref-point)))
    (let ((px
	   (-
	    (* (cos theta) (- (point-x point-to-rotate) (point-x ref-point)))
	    (* (sin theta) (- (point-y point-to-rotate) (point-y ref-point)))
	    (- (point-x ref-point))))
	  (py
	   (+
	    (* (sin theta) (- (point-x point-to-rotate) (point-x ref-point)))
	    (* (cos theta) (- (point-y point-to-rotate) (point-y ref-point)))
	    (point-y ref-point))))
      (setf (point-x point-to-rotate) px)
      (setf (point-y point-to-rotate) py)))

(defmethod geometry->points ((p point))
  (list p))

(defmethod closest-point-to-set ((p point) (point-set list))
  (with-valid-geometry ((append (list p) point-set))
    (loop
       with distance = nil
       with closest-point = nil
       for other-point in point-set
       do
	 (let ((d (distance p other-point)))
	   (when (or (eq distance nil)
		     (< d distance))
	     (setf closest-point other-point)
	     (setf distance d)))
       finally
	 (return closest-point))))

(defun dump-point-set (pset filename)
  (with-open-file (stream filename :direction :output :if-does-not-exist :create :if-exists :supersede)
    (format stream "X, Y, Z~%")
    (loop
       for p in pset
       do
	 (format stream "~F, ~F, ~F~%" (point-x p) (point-y p) (point-z p)))))
