(in-package :geom)

(defparameter *dc-permutations* (comb:permutations-with-repetition '(-1 0 1) 4))

(defmethod point-along-polyline ((p polygon) (d number) &optional (starting-point nil))
  "Returns the point that is a distance d away from the starting point along the polygon p"
  (unless starting-point
    (setf starting-point (first (polygon-points p))))

  (cond ((= d 0.0)
	 starting-point)
	((= d (polygon-perimeter p))
	 (end-point (first (last (polygon-lines p)))))
	((> d (polygon-perimeter p))
	 (error 'geometry-error 
		:geometry-object p 
		:error-text "Distance along polyline must be shorter than polyline length."))
	((< d 0)
	 (error 'geometry-error 
		:geometry-object p 
		:error-text "Distance along polyline must be positive."))
	(t
	 (loop
	    with distance = 0
	    with result-point = nil
	    for ls in (polygon-lines p)
	    while (< distance d)
	    if (< (+ (line-segment-length ls) distance) d)
	    do
	      ;;(print distance)
	      (setf distance (+ distance (line-segment-length ls)))
	    else
	    do
	      ;;(print ls)
	      (let* (;;(ls-sp (start-point ls))
		     (dist-along-ls (- d distance))
		     (int-point (intermediate-point ls dist-along-ls)))
		(setf distance d)
		(setf result-point int-point))
	    finally
	      (return result-point)))))


(defmethod generalized-polyline ((p polygon) (n fixnum) &optional (starting-point nil))
  (unless starting-point
    (setf starting-point (first (polygon-points p))))
  (make-polygon
   (remove-if #'(lambda (x) (eq nil x))
	      (loop
		 with lim = (if (polygon-closed? p)
				(- (expt 2 n) 1)
				(expt 2 n))

		 with polyline-length = (polygon-perimeter p)
		 for i from 0 to lim
		 collect
		   (point-along-polyline p (* (/ i lim) polyline-length))))
   (polygon-closed? p)))

(defmethod dc-coefficients ((ls1 line-segment) (ls2 line-segment))
  (let* ((x-i (point-x (end-point ls1)))
	 (y-i (point-y (end-point ls1)))
	 (x-i-1 (point-x (start-point ls1)))
	 (y-i-1 (point-y (start-point ls1)))
	 (x-j (point-x (end-point ls2)))
	 (y-j (point-y (end-point ls2)))
	 (x-j-1 (point-x (start-point ls2)))
	 (y-j-1 (point-y (start-point ls2)))

	 (c1 (- 
	      (sgn-numeric
	       (+
		(* (- x-j-1 x-i-1) (- x-i x-i-1))
		(* (- y-j-1 y-i-1) (- y-i y-i-1))))))

	 (c2 (sgn-numeric
	      (+
	       (* (- x-j-1 x-i-1) (- x-j x-j-1))
	       (* (- y-j-1 y-i-1) (- y-i y-j-1)))))

	 (c3 (- 
	      (sgn-numeric
	       (-
		(* (- x-j-1 x-i-1) (- y-i y-i-1))
		(* (- y-j-1 y-i-1) (- x-i x-i-1))))))
	 
	 (c4 (sgn-numeric
	      (-
	       (* (- x-j-1 x-i-1) (- y-j y-j-1))
	       (* (- y-j-1 y-i-1) (- x-j x-j-1))))))
    (list c1 c2 c3 c4)))

(defmethod dc-matrix ((p polygon))
  "Create the double-cross matrix of the polygon."
  ;; TODO: take advantage of the symmetrical nature of the DC coefficients to
  ;; avoid computing unnecessary ones. For now, just do it brute force.
  (loop
     for ls1 in (polygon-lines p)
     collect
       (loop
	  for ls2 in (polygon-lines p)
	  collect
	    (dc-coefficients ls1 ls2))))

(defun dc-similarity-h (dcm1 dcm2)
  "Compute the similarity between two double-cross matrices. This is the \Delta_{H}
metric from the original Kuijpers et al. paper."
  (assert (= (length dcm1) (length dcm2)))
  (let ((n (length dcm1))
	(dc-coeffs-count-1 (count-dc-coeffs dcm1))
	(dc-coeffs-count-2 (count-dc-coeffs dcm2)))
    (coerce
     (*
      (/ 1 (- (expt n 2) n))
      (apply #'+ (mapcar #'abs (mapcar #'- dc-coeffs-count-1 dc-coeffs-count-2))))
     'float)))
  

(defun delta-sim (coeffs1 coeffs2)
  (apply #'+ (mapcar #'abs (mapcar #'- coeffs1 coeffs2))))

(defun dc-similarity-e (dcm1 dcm2)
  (assert (= (length dcm1) (length dcm2)))
  (let* ((flat-dcm1 (loop for x in dcm1 append x))
	 (flat-dcm2 (loop for x in dcm2 append x))
	 (n (length dcm1)))
    (loop
       for x in flat-dcm1
       for y in flat-dcm2
       ;;do (format t "~A ~A ~F~%" x y (delta-sim x y))
       sum (delta-sim x y) into sim
       finally (return (coerce (/ sim (* 4 (expt (- n 1) 2))) 'float)))))

(defun count-dc-coeffs (dcm)
  (let ((flat-dcm (loop for x in dcm append x)))
    (loop
       for k in *dc-permutations*
       collect
	 (count k flat-dcm :test #'equalp))))

(defmethod polyline-similarity ((p1 polygon) (p2 polygon) &key (epsilon 0.01) (simfunc #'dc-similarity-e))
  (loop
     for n upfrom 3
     with current-p1 = (generalized-polyline p1 2)
     with current-p2 = (generalized-polyline p2 2)
     while (or (>= (abs (- (polygon-perimeter current-p1) (polygon-perimeter p1))) (* epsilon (polygon-perimeter p1)))
	       (>= (abs (- (polygon-perimeter current-p2) (polygon-perimeter p2))) (* epsilon (polygon-perimeter p2))))
     do
       (setf current-p1 (generalized-polyline p1 n))
       (setf current-p2 (generalized-polyline p2 n))
       ;;(print current-p1)
       ;;(print current-p2)
       (format t "~F ~F~%" (polygon-perimeter current-p1) (polygon-perimeter p1))
       (format t "~F ~F~%" (polygon-perimeter current-p2) (polygon-perimeter p2))
       ;;(print n)
     finally
       (let ((dcm1 (dc-matrix current-p1))
	     (dcm2 (dc-matrix current-p2)))
	 (return (funcall simfunc dcm1 dcm2)))))

(defun get-matrix-value (matlist i j)
  (when (and (< i (length matlist))
	     (< j (length (first matlist))))
    (nth j (nth i matlist))))

(defmethod dump-as-python-arrays ((p polygon) stream)
  (let ((lines (polygon-lines p)))
    (format stream "[~{~3,6F~^, ~}]~%" (apply #'append (mapcar #'(lambda (x)
								   (list (point-x (start-point x))
									 (point-x (end-point x))))
							       lines)))
    (format stream "[~{~3,6F~^, ~}]~%" (apply #'append (mapcar #'(lambda (x)
								   (list (point-y (start-point x))
									 (point-y (end-point x))))
							       lines)))))

(defmethod match-polyline-to-polygon ((p-line polygon) (p-target polygon) &key (epsilon 0.01) (simfunc #'dc-similarity-e))
  (let ((num-q-segments (length (polygon-lines p-line)))
	(num-t-segments (length (polygon-lines p-target))))
    (loop
       for i from 0 to num-t-segments
       minimize (polyline-similarity p-line (polygon-subsection p-target i num-q-segments) 
				     :epsilon epsilon :simfunc simfunc)
       into best-match-sim
       finally (return best-match-sim))))
