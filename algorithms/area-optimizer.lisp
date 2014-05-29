(in-package :geom)

(defmethod optimize-area ((p polygon) (area number) (num-points integer) &key (starting-point nil) (epsilon 0.01) )
  "Add points to the polyline to match the given area."
  (unless starting-point
    (setf starting-point (centroid p)))
  (format t "~F ~F~%" (polygon-area p) area)

  (loop
     with candidate-points = (loop
				for i from 0 to (- num-points 1)
				collect (make-point (point-x starting-point)
						    (point-y starting-point)
						    (point-z starting-point)))
     with current-polygon = p
     with step-radius = (loop
			   for cp in candidate-points
			   minimize (apply #'min (mapcar #'(lambda (ls)
							     (distance ls cp))
							 (polygon-lines current-polygon)))
			   into sr
			   finally (return sr))
     for iter upfrom 0
     while
	(> (abs (- (polygon-area current-polygon) area)) (* epsilon area))
     do
       (format t "candidates: ~A~%" candidate-points)
       (let* ((new-points
	       (loop
		  for pt in candidate-points
		  collect
		    (let* ((random-points (random-points-in-circle pt step-radius 25))
			   (best-point (argmin #'(lambda (x)
						   (abs (- (polygon-area (make-polygon (append (polygon-points p)
											       (list x))))
							   area)))
					       random-points)))
		      ;;(print random-points)
		      best-point))))
	 (format t "step radius: ~F~%" step-radius)
	 (format t "new points: ~A~%" new-points)
	 (setf current-polygon (make-polygon (append (polygon-points p)
						     new-points)))
	 (setf step-radius (min (loop
				   for pt in candidate-points
				   maximize
				     (apply #'max (mapcar #'(lambda (x)
							      (distance x pt))
							  new-points)))
				step-radius))
	 (format t "new polygon: ~A~%area: ~F~%centroid: ~A~%"
		 current-polygon 
		 (polygon-area current-polygon)
		 (centroid current-polygon))
	 (setf candidate-points new-points))
     finally
       (return candidate-points)))
		       


(defmethod random-point-on-circle ((p point) (radius number))
  "Generate a random point on the boundary of a circle with specified radius."
  (let* ((theta (random (* 2 pi)))
	 (delta-x (* radius (cos theta)))
	 (delta-y (* radius (sin theta))))
    (make-point (+ (point-x p) delta-x)
		(+ (point-y p) delta-y)
		(point-z p))))

(defmethod random-point-in-circle ((p point) (radius number))
  "Generate a random point in the interior of a circle with specified radius."
  (let* ((theta (random (* 2 pi)))
	 (p-rad (sqrt (random radius)))
	 (delta-x (* p-rad (cos theta)))
	 (delta-y (* p-rad (sin theta))))
    (make-point (+ (point-x p) delta-x)
		(+ (point-y p) delta-y)
		(point-z p))))


(defmethod random-points-on-circle ((p point) (radius number) (num-points integer))
  "Generate a set of points randomly distributed around the perimeter of a circle with given radius."
  (loop
     for i from 0 to num-points
     collect (random-point-on-circle p radius)))

(defmethod random-points-in-circle ((p point) (radius number) (num-points integer))
  "Generate a set of points randomly distributed through the interior of a circle with given radius."
  (loop
     for i from 0 to num-points
     collect (random-point-in-circle p radius)))


    
(defun argmin (func list-of-items)
  "Find the item (plus its index) in list-of-items that minimizes func."
  (loop
     with current-func-value = nil
     with current-best-item = nil
     with current-best-index = nil
     for item in list-of-items
     for i upfrom 0
     do
       (let* ((new-func-value (funcall func item)))
	 (if (or (null current-func-value)
		 (null current-best-item)
		 (null current-best-index)
		 (< new-func-value current-func-value))
	     (progn
	       (setf current-func-value new-func-value)
	       (setf current-best-item item)
	       (setf current-best-index i))))
     finally
       (return (values current-best-item current-best-index))))
	     

(defun argmax (func list-of-items)
  "Find the item (plus its index) in list-of-items that maximizes func."
  (loop
     with current-func-value = nil
     with current-best-item = nil
     with current-best-index = nil
     for item in list-of-items
     for i upfrom 0
     do
       (let* ((new-func-value (funcall func item)))
	 (if (or (null current-func-value)
		 (null current-best-item)
		 (null current-best-index)
		 (> new-func-value current-func-value))
	     (progn
	       (setf current-func-value new-func-value)
	       (setf current-best-item item)
	       (setf current-best-index i))))
     finally
       (return (values current-best-item current-best-index))))
