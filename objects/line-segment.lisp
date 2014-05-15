(in-package :geom)

(defclass line-segment (geometric-object)
  ((start-point :accessor start-point
		:initform (make-instance 'point)
		:initarg :start-point)
   (end-point   :accessor end-point
		:initform (make-instance 'point)
		:initarg :end-point)
   (length      :accessor line-segment-length
		:initform nil
		:initarg :length)
   (m           :accessor line-segment-m
		:initform nil
		:initarg :m)
   (b           :accessor line-segment-b
		:initform nil
		:initarg :b)
   (nx          :accessor line-segment-nx
		:initform nil
		:initarg :nx)
   (ny          :accessor line-segment-ny
		:initform nil
		:initarg :ny)))

(defgeneric make-line (start-point end-point))

(defmethod make-line ((start-point point) (end-point point))
  (make-instance 'line-segment 
		 :start-point start-point 
		 :end-point end-point))

(defmethod initialize-instance :after ((ls line-segment) &rest args)
  (declare (ignore args))
  (when (and (is-geom-valid? (start-point ls))
	     (is-geom-valid? (end-point ls)))
    ;;(print (start-point ls))
    ;;(print (end-point ls))
    (let* ((pd (difference (start-point ls) (end-point ls)))
	   (m
	    (if (not (= (point-x pd) 0))
		(/ (point-y pd) (point-x pd))
		nil))
	   (b
	    (if (not (eq m nil))
		(- (point-y (start-point ls)) (* m (point-x (start-point ls))))
		nil))
	   (l (sqrt (+ (expt (point-x pd) 2) (expt (point-y pd) 2))))
	   (nx (- (point-y pd)))
	   (ny (point-x pd)))
      (setf (line-segment-m ls) m)
      (setf (line-segment-b ls) b)
      (setf (line-segment-nx ls) nx)
      (setf (line-segment-ny ls) ny)
      (setf (line-segment-length ls) l))))


(defmethod compute-line-parameters ((ls line-segment))
  (let* ((pd (difference (start-point ls) (end-point ls)))
	 (m
	  (if (not (= (point-x pd) 0))
	      (/ (point-y pd) (point-x pd))
	      nil))
	 (b
	  (if (not (eq m nil))
	      (- (point-y (start-point ls)) (* m (point-x (start-point ls))))
	      nil))	 
	 (l (sqrt (+ (expt (point-x pd) 2) (expt (point-y pd) 2))))
	 (nx (- (point-y pd)))
	 (ny (point-x pd)))
  (setf (line-segment-m ls) m)
  (setf (line-segment-b ls) b)
  (setf (line-segment-nx ls) nx)
  (setf (line-segment-ny ls) ny)
  (setf (line-segment-length ls) l)))

(defmethod set-endpoint-coords ((ls line-segment) (start-point point) (end-point point))
  "Set the coordinates of the line segment endpoints without overwriting the underlying objects."
  (set-coords (start-point ls) start-point)
  (set-coords (end-point ls) end-point)
  (compute-line-parameters ls))
  
  
(defmethod set-endpoints ((ls line-segment) (start-point point) (end-point point))
  "Set the endpoints of the line segment. It's important to note that this does not *just* set
the endpoint coordinates, but actually sets the objects passed as endpoints. To set the
coordinates of the endpoints use set-endpoint-coords."
  (setf (start-point ls) start-point)
  (setf (end-point ls) end-point)
  (compute-line-parameters ls))

(defmethod geom= ((ls1 line-segment) (ls2 line-segment) &key (tolerance 0.01))
  (when (or (and (geom= (start-point ls1) (start-point ls2) :tolerance tolerance)
		 (geom= (end-point ls1) (end-point ls2) :tolerance tolerance))
	    (and (geom= (start-point ls1) (end-point ls2) :tolerance tolerance)
		 (geom= (end-point ls1) (start-point ls2) :tolerance tolerance)))
    t))

(defmethod other-end ((ls line-segment) (p point))
  "If the point passed represents either end of the line segment, return the *other* end that
is not the argument. If some random point is passed in, return nil."
  (cond ((eq p (start-point ls))
	 (end-point ls))
	((eq p (end-point ls))
	 (start-point ls))
	(t nil)))

(defmethod is-geom-valid? ((ls line-segment))
  (cond ((and (is-geom-valid? (start-point ls))
	      (is-geom-valid? (end-point ls))
	      (not (eq (line-segment-length ls) nil)))
	 t)
	(t nil)))
	

(defmethod centroid ((list-of-segments list))
  (with-valid-geometry (list-of-segments)
    (let ((points
	   (loop
	      for seg in list-of-segments
	      append (list (start-point seg) (end-point seg)))))
      (point-centroid points))))

(defmethod geometry->stl ((ls line-segment) &optional (terminator t))
  (let* ((sp (start-point ls))
	 (ep (end-point ls))
	 (ll (make-point (point-x sp) (point-y sp) 0.0))
	 (ul (make-point (point-x sp) (point-y sp) (or (point-z sp) 0.0)))
	 (lr (make-point (point-x ep) (point-y ep) 0.0))
	 (ur (make-point (point-x ep) (point-y ep) (or (point-z ep) 0.0))))
    (format nil "~:[~;[~] ~{~A~^, ~} ~:[~;]~]" terminator (geometry->stl (list ll ul lr ur)) terminator)))

(defmethod print-object ((ls line-segment) stream)
  (format stream "{~S -> ~S}" (start-point ls) (end-point ls)))

(defmethod intersect? ((ls1 line-segment) (ls2 line-segment))
  (with-valid-geometry ((list ls1 ls2))
    (let* ((m1 (or (line-segment-m ls1)
		   (/ 
		    (- (point-y (end-point ls1)) (point-y (start-point ls1)))
		    (- (point-x (end-point ls1)) (point-x (start-point ls1))))))
	   (m2 (or (line-segment-m ls2)
		   (/ 
		    (- (point-y (end-point ls2)) (point-y (start-point ls2)))
		    (- (point-x (end-point ls2)) (point-x (start-point ls2)))))))
      (if (= m1 m2)
	  (values nil (make-point))
	  (progn
	    (let* ((b1 (-
			(point-y (end-point ls1))
			(* m1 (point-x (end-point ls1)))))
		   (b2 (-
			(point-y (end-point ls2))
			(* m2 (point-x (end-point ls2)))))
		   (x-int (/
			   (- b2 b1)
			   (- m1 m2)))
		   (y-int (+ (* m1 x-int) b1)))

	      (if (and (>< x-int (point-x (start-point ls1)) (point-x (end-point ls1)))
		       (>< x-int (point-x (start-point ls2)) (point-x (end-point ls2)))
		       (>< y-int (point-y (start-point ls1)) (point-y (end-point ls1)))
		       (>< y-int (point-y (start-point ls2)) (point-y (end-point ls2))))
		  (values t (make-point x-int y-int))
		  (values nil (make-point)))))))
    (values nil (make-point))))

(defmethod copy-geometry ((ls line-segment))
  (let ((sp (copy-geometry (start-point ls)))
	(ep (copy-geometry (end-point ls))))
    (make-line sp ep)))

(defun swap-ends (ls)
  (let ((temp-start (start-point ls)))
    (setf (start-point ls) (end-point ls))
    (setf (end-point ls) temp-start)))

;; some algorithms specific to line segments to help with sorting

(defmethod common-corners ((ls1 line-segment) (ls2 line-segment) &key (corner-radius 1.0))
  "Return the common corners of ls1 and ls2 in a list. Element 1 is the ls1 corner,
element 2 is the ls2 corner (1-indexed) etc."
  (with-valid-geometry ((list ls1 ls2))
    (let ((corners1 (list (start-point ls1) (end-point ls1)))
	  (corners2 (list (start-point ls2) (end-point ls2))))
      (loop
	 for c1 in corners1
	 append
	   (loop
	      for c2 in corners2
	      if (<= (distance c1 c2) corner-radius)
	      append
		(list c1 c2))))))

(defmethod line-segment->vector ((ls line-segment))
  (v- (point->vector (end-point ls))
      (point->vector (start-point ls))))

;;(defmethod line-segment->vector ((list-of-segments list))
;;  (mapcar #'
	   
(defmethod line-segment-midpoint (ls)
  (with-valid-geometry (ls)
    (point-centroid (list (start-point ls) (end-point ls)))))

(defun segment-to-point-distances (list-of-segments ref-point)
  (loop
     for ls in list-of-segments
     if (is-geom-valid? ls)
     collect
       (list ls (distance (line-segment-midpoint ls) ref-point))))

(defun closest-segment-to-point (list-of-segments ref-point)
  (first (first
	  (sort
	   (segment-to-point-distances list-of-segments ref-point)
	   #'< :key #'second))))

(defun farthest-segment-from-point (list-of-segments ref-point)
  (first (first
	  (sort
	   (segment-to-point-distances list-of-segments ref-point)
	   #'> :key #'second))))

(defmethod intermediate-point ((ls line-segment) (d number))
  "Return the coordinates of a point that is a distance d away from the start-point of the segment."
  (let* ((tp (/ d (line-segment-length ls)))
	 (a (point-x (start-point ls)))
	 (b (point-y (start-point ls)))
	 (c (point-x (end-point ls)))
	 (d (point-y (end-point ls)))
	 (x (+ (* (- 1 tp) a) (* tp c)))
	 (y (+ (* (- 1 tp) b) (* tp d))))
    (make-point x y 0)))

(defmethod extend-line ((ls line-segment) (extend-by number))
  "Extend a line by a given distance beyond its end-point."
  (let* ((new-line (copy-geometry ls)))
    (extend-line-in-place new-line extend-by)
    new-line))

(defmethod extend-line* ((ls line-segment) (extend-by number))
  (let* ((dx (- (point-x (end-point ls)) (point-x (start-point ls))))
	 (dy (- (point-y (end-point ls)) (point-y (start-point ls))))
	 (dz (- (point-z (end-point ls)) (point-z (start-point ls))))
	 (l (line-segment-length ls))
	 (cx (+ (point-x (end-point ls)) (* (/ dx l) extend-by)))
	 (cy (+ (point-y (end-point ls)) (* (/ dy l) extend-by)))
	 (cz (+ (point-z (end-point ls)) (* (/ dz l) extend-by))))
    (set-coords (end-point ls) `(,(cons :x cx) ,(cons :y cy)))))


(defmethod reflect-object ((ls line-segment) (ref-line line-segment))
  (let* ((sp (start-point ls))
	 (ep (end-point ls)))
    (reflect-object sp ref-line)
    (reflect-object ep ref-line)))

(defmethod geometry->points ((ls line-segment))
  (list (start-point ls) (end-point ls)))

(defmethod subdivide-into-points ((ls line-segment) (n fixnum))
  (let* ((sp (start-point ls))
	 (ep (end-point ls))
	 (dx (/ (- (point-x ep) (point-x sp)) n))
	 (dy (/ (- (point-y ep) (point-x sp)) n))
	 (dz (/ (- (point-z ep) (point-z sp)) n)))
    (loop
       for i from 0 to n
       collect
	 (make-point (+ (point-x sp) (* i dx))
		     (+ (point-y sp) (* i dy))
		     (+ (point-z sp) (* i dz))))))
