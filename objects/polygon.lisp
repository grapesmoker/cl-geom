(in-package :geom)

(defclass polygon (geometric-object)
  ((points :accessor polygon-points
	   :initform nil
	   :initarg :points)
   (lines  :accessor polygon-lines
	   :initform nil
	   :initarg :lines)
   (orientation :accessor polygon-orientation
		:initform nil
		:initarg orientation)))

(defun make-polygon (points-or-lines &optional (closed t) (tolerance 1e-8))
  "Construct a polygon from the supplied points or lines. The argument must be a list
with at least 3 entries which must be either ALL points or ALL lines. If the closed 
parameter is set to nil, the final point of the polygon will not be joined with the
first point, resulting in a polyline."
  (when (and (< (length points-or-lines) 3)
	     (every #'(lambda (x)
			(eq 'geom:point (type-of x)))
		    points-or-lines))
    (error 'geometry-error :error-text "Need at least 3 points to make a polygon!"))
  (with-valid-geometry (points-or-lines)
    (let ((new-polygon (make-instance 'polygon)))
      ;;(setf points-or-lines (reverse points-or-lines))
      (cond ((every #'(lambda (x)
			(eq 'geom:point (type-of x)))
		    points-or-lines)
	     (loop
		with previous-point = (first points-or-lines)
		initially
		  (push previous-point (polygon-points new-polygon))
		for p in (rest points-or-lines)
		do
		  (pushnew p (polygon-points new-polygon) :test #'(lambda (x y) (geom= x y :tolerance 1e-8)))
		  (push (make-line previous-point p) (polygon-lines new-polygon))
		  (setf previous-point p)
		finally
		  (when closed
		    (push (make-line p (car (last (polygon-points new-polygon)))) (polygon-lines new-polygon)))))
	    ((every #'(lambda (x)
			(eq 'geom:line-segment (type-of x)))
		    points-or-lines)
	     (loop
		for l in points-or-lines
		do
		  (push l (polygon-lines new-polygon))
		  (pushnew (start-point l) (polygon-points new-polygon) 
			   :test #'(lambda (x y) (geom= x y :tolerance tolerance)))
		  (pushnew (end-point l) (polygon-points new-polygon)
			   :test #'(lambda (x y) (geom= x y :tolerance tolerance)))))
	    (t (error 'geometry-error :error-text "The argument must be either all points or all lines!")))
      (setf (polygon-lines new-polygon) (reverse (polygon-lines new-polygon)))
      (setf (polygon-points new-polygon) (reverse (polygon-points new-polygon)))
      new-polygon)))

(defun load-polygon-from-file (filename)
  (with-open-file (stream filename)
    (make-polygon 
     (loop
	for line = (read-line stream nil nil)
	while line
	collect
	  (let ((coords 
		 (mapcar #'read-from-string 
			 (remove-if #'(lambda (x) (string= x ""))
				    (cl-ppcre:split "\\s+" line)))))
	    (make-point (first coords) (second coords) 0))))))

(defmethod polygon-subsection ((p polygon) (start-segment fixnum) (number-of-segments fixnum) &optional (closed nil))
  "Return a polyline beginning with the segment indexed start and ending with the segment end, inclusive."
  (cond ((> number-of-segments (length (polygon-lines p)))
	 (make-polygon (polygon-lines p)))
	((> (+ start-segment number-of-segments) (length (polygon-lines p)))
	 (make-polygon (subseq (comb:rotate-list (polygon-lines p)
						 (- start-segment (length (polygon-lines p))))
			       0 number-of-segments)))
	(t 
	 (make-polygon (subseq (polygon-lines p) start-segment (+ start-segment number-of-segments)) closed))))

(defmethod polygon->graph ((pol polygon))
  "Transform the polygon into a graph whose nodes are points and whose
weighted edges are line segments connecting those points"
  (let ((g (cl-graph:make-graph 'cl-graph:graph-container)))
    (dolist (l (polygon-lines pol))
      (cl-graph:add-edge-between-vertexes g (start-point l) (end-point l) :value (line-segment-length l)))
    g))

(defmethod polygon-closed? ((p polygon))
  (or (eq (start-point (first (polygon-lines p)))
	  (end-point (first (last (polygon-lines p)))))
      (geom= (start-point (first (polygon-lines p)))
	     (end-point (first (last (polygon-lines p)))))))
  

(defmethod geometry->stl ((p polygon) &optional (terminator t))
  (let ((line-segments (polygon-lines p)))
    (format nil  "~:[~;[~] ~{~A~^, ~} ~:[~;]~]"
	    terminator
	    (loop
	       for ls in line-segments
	       collect
		 (geometry->stl ls nil))
	    terminator)))

(defmethod polygon-perimeter ((p polygon))
  (apply #'+ (mapcar #'line-segment-length (polygon-lines p))))

(defmethod polygon-area ((p polygon))
  (loop
     with first-point = (first (polygon-points p))
     with prev-point = (first (polygon-points p))
     for current-point in (rest (polygon-points p))
     summing
       (- (* (point-x prev-point) (point-y current-point))
	  (* (point-x current-point) (point-y prev-point)))
     into area
     do
       (setf prev-point current-point)
     finally
       (return (abs (* 0.5 (+ area 
			      (- (* (point-x current-point) (point-y first-point))
				 (* (point-x first-point) (point-y current-point)))))))))



(defmethod normalize-polygon-lengths ((p polygon) &optional (norm-factor (* 2 pi)))
  (mapcar #'(lambda (x) (/ x (/ (curve-length p (length (polygon-lines p))) norm-factor)))
	  (mapcar #'line-segment-length (polygon-lines p))))


(defmethod edge-index ((p polygon) (a number))
 (car
  (mapcar #'car
	  (remove-if #'(lambda (x)
			 (< (cdr x) a))
		     (loop
			for segment-length in (normalize-polygon-lengths p)
			for k upfrom 0
			summing segment-length into sum-k
			collect (cons k sum-k))))))

(defmethod curve-length ((p polygon) (i number))
  (loop
     for segment in (polygon-lines p)
     for j below i
     summing (line-segment-length segment) into sum
     finally (return sum)))

(defmethod normalized-curve-length ((p polygon) (i number) &optional (norm-factor (* 2 pi)))
  (loop
     for segment-length in (normalize-polygon-lengths p norm-factor)
     for j below i
     summing segment-length into sum
     finally (return sum)))

(defmethod parametrize-polygon ((p polygon) (tp number))
  (let* ((edge-bound (edge-index p tp))
	 (p-lines (polygon-lines p))
	 (vecs-to-sum (mapcar #'line-segment->vector
			      (subseq p-lines 0 edge-bound))))
    ;;(format t "~A~%" vecs-to-sum)
    (v+
     (if vecs-to-sum
	 (apply #'vec-sum vecs-to-sum)
	 (make-vec3 0 0 0))
     (v* 
      (line-segment->vector (nth edge-bound (polygon-lines p)))
      (- tp (normalized-curve-length p edge-bound))))))
      

(defmethod geometry->points ((p polygon))
  (polygon-points p))


(defmethod centroid ((p polygon))
  (point-centroid (polygon-points p)))

(defmethod rotate-object ((p polygon) (angle number) (ref-point point))
  (rotate-objects (polygon-points p) angle ref-point)
  (rotate-objects (polygon-lines p) angle ref-point))

(defmethod print-object ((p polygon) stream)
  (format stream "<#POLYGON (~{~A~^~% ~})>" (polygon-lines p)))


(defmethod dump-object ((p polygon) (output-file string))
  (with-open-file (stream output-file :if-exists :supersede :direction :output)
    (format stream "~{~A~^,~}~%" '(x-start y-start x-end y-end))
    (loop
       for ls in (polygon-lines p)
       do
	 (format stream "~{~,2F~^,~}~%" (list (point-x (start-point ls))
					      (point-y (start-point ls))
					      (point-x (end-point ls))
					      (point-y (end-point ls)))))))

(defmethod reflect-object ((p polygon) (ref-line line-segment))
  (reflect-objects (polygon-lines p) ref-line)
  (reflect-objects (polygon-points p) ref-line))


(defmethod set-height ((p polygon) (value number))
  (loop
     for pt in (polygon-points p)
     do
       (set-height pt value)))

(defmethod is-valid? ((p polygon))
  (loop
     with valid-flag = t
     with current-ls = (first (polygon-lines p))
     for ls in (rest (polygon-lines p))
     if (not (eq (end-point current-ls)
		(start-point ls)))
     do (setf valid-flag nil)
     do (setf current-ls ls)
     finally
       (return valid-flag)))
