(in-package :geom)

(defun point-pairings (data-points model-points)
  "Generate all possible pairings of data points and model points. Basically the Cartesian product."
  (loop
     for d in data-points
     append
       (loop
	  for m in model-points
	  collect `(,d ,m))))

(defun multipass-icp (data-points model-points &key (threshold 0.01) (method :svd) (robot-coords nil) (vis-penalty 0.25))
  "The data is so sparse that ICP gets stuck in local minima. The idea here is to take every possible aligned
subset of the model and find the match that gets us the closest. The method parameter indicates what method
to use; either :svd (singular value decomposition) or :quaternion are acceptable. In 2D, SVD seems to work
best, converging almost instantaneously."
  ;; The idea behind the robot-coords parameter is to penalize orientations that end up too close
  ;; to the robot. There can be two matching orientations, degenerate to 180.0. But no transformed
  ;; point should be visible to the robot because otherwise it would be a data point. For every 
  ;; such point, there should be a penalty incurred; what that penalty shoudl be is not obvious,
  ;; but 1.0 seems reasonable.
  ;; -JV 12-10-13
  (let* ((num-data-points (length data-points))
	 (num-model-points (length model-points))
	 (data-copy (loop for p in data-points collect (copy-geometry p)))
	 (data-model-alignments
	  (append
	   (loop
	     for i from 0 to (- num-model-points 1)
	     collect
	       (list data-points (subseq (cl-user::rotate-list model-points i) 0 (min num-data-points (length model-points)))))
	   (loop
	      for i from 0 to (- num-model-points 1)
	      collect
		(list (reverse data-points) (subseq (cl-user::rotate-list model-points i) 0 (min num-data-points (length model-points))))))))
	 ;;(num-alignments (length data-model-alignments)))
    ;; (format t "data-model alignments: ~A~%" data-model-alignments)
    (loop
       with current-error = 1e10
       with current-params = nil
       for counter upfrom 1
       for data-model-pairing in data-model-alignments
       do
	 ;; (format t "data set: ~A~%" (first data-model-pairing))
	 ;; (format t "model set: ~A~%" (second data-model-pairing))
         (log:debug "*************************** (start of icp cycle) *****************************")
         (log:debug "current-error at start of pass: ~F" current-error)
	 (let* ((data 
                 (mapcar #'copy-geometry (first data-model-pairing)))
		(model (second data-model-pairing))
		(icp-steps (icp data model :threshold threshold :method method))
		(final-error (third (first (last icp-steps)))))
	   (log:debug "data: ~A" data-copy)
           (log:debug "model: ~A" model)
	   (log:debug "counter: ~D" counter)
	   ;; (format t "result: ~A~%" icp-steps)
	   ;; here we check to see if we got the `wrong' orientation and penalize accordingly
	   (when robot-coords
	     (log:debug "checking to make sure we didn't end up too close to the robot")
	     (let* ((model-copy 
                     (remove-if #'(lambda (x) 
                                    (member x model :test #'geom=))
                                (mapcar #'copy-geometry model-points)))
		    (visible-segments 
		     (loop
			with previous-point = nil
			for p in data-copy
			when previous-point
			collect
			  (make-line previous-point p)
			do
			  (setf previous-point p))))
	       (log:debug "visible segments: ~A" visible-segments)
	       (apply-inverse-transformation-list icp-steps model-copy)
	       ;; remove any results that overlap with data we already have
	       ;; the idea is to have only the points left over that are from
	       ;; the model
	       ;; (setf model-copy
	       ;; 	     (remove-if #'(lambda (x)
	       ;; 			    (member x data-copy :test #'geom=))
	       ;; 			model-copy))
	       (let* ((penalty
		       (loop
			  for mp in model-copy
			  do (log:debug "point ~A visible from ~A?: ~A"
				     mp robot-coords (is-point-visible? mp robot-coords visible-segments))
			  if (is-point-visible? mp robot-coords visible-segments)
			  sum vis-penalty into penalty
			  finally (return penalty))))
		 (log:debug "penalized this orientation by ~F" penalty)
		 (setf final-error (+ final-error penalty)))))
	      
	   (if (< final-error current-error)
	       (progn
		 (setf current-error final-error)
		 (setf current-params icp-steps)
                 (log:debug "current params: ~A" current-params)
                 ;;(setf (third current-params) final-error)
		 (log:debug "final error: ~F" current-error))))
       finally (return (values current-params current-error)))))
	      

(defun icp (data-points model-points &key 
	    (initial-translation-guess (make-vect '(0 0 0)))
	    (initial-rotation-guess (make-quaternion 1 0 0 0))
	    (threshold 0.05)
	    (method :svd))
  "Implements the iterative closest point algorithm. Returns a list of triples of all transformations until
convergence plus the associated error at each step, e.g. (quaternion translation error). The method parameter
controls whether the transformation will be computed using SVD or via the quaternion method. They both seem
to work but quaternion is a bit unstable, while SVD converges really fast, so SVD should be used in 2D for
our purposes."

  ;; NOTE: This function computes a whole mess of shit, some of which is not necessary if you go with
  ;; SVD rather than quaternions. Ideally, those computations should be broken out into separate
  ;; functions and made conditional on the :method parameter, but I don't have the time to do this now.
  ;; -JV 12-09-13

  (declare (ignore initial-rotation-guess initial-translation-guess))
  (loop
     with dms-error = nil
     with previous-dms-error = nil
     with retval = nil
     with stop-flag = nil
     until stop-flag
     do
       (let* ((point-correspondences
	       (loop
		  for dp in data-points
		  for mp in model-points
		  collect
		    (list dp mp)))
	       ;; (loop
	       ;; 	  with remaining-model-points = model-points
	       ;; 	  for point in data-points
	       ;; 	  collect 
	       ;; 	    (let* ((closest-model-point (closest-point-to-set point remaining-model-points)))
	       ;; 	      (setf remaining-model-points (remove closest-model-point remaining-model-points))
	       ;; 	      (list point closest-model-point))))
	      (num-data-points (length data-points))
	      ;; (current-error
	      ;;  (loop
	      ;; 	  for pair in point-correspondences
	      ;; 	  summing (distance (first pair) (second pair))
	      ;; 	  into error
	      ;; 	  finally (return (/ error num-data-points))))
	      (data-centroid (point-centroid data-points))
	      (model-centroid (point-centroid model-points))
	      (data-centroid-v (point->vector data-centroid))
	      (model-centroid-v (point->vector model-centroid))
	      (ident (ident-matrix 3))
	      ;;(ident (make-matrix 3 3 '((1 0 0) (0 1 0) (0 0 1))))
	      ;;(ident (make-array '(3 3) :element-type 'number :initial-contents '((1 0 0) (0 1 0) (0 0 1))))
	      (cross-covariance
	       (mult-matrix
		(loop
		   with result-tp = (make-matrix 3 3)
		   for pair in point-correspondences
		   do
		     (let* ((dp (v+ (point->vector (first pair))
				    (v* data-centroid-v -1)))
			    (mp (v+ (point->vector (second pair))
				    (v* model-centroid-v -1)))
			    (tp (tensor-product dp mp)))
		       (setf result-tp (m+ result-tp tp)))
		   finally (return result-tp))
		(/ num-data-points)))
	      (cross-covariance-t (transpose-matrix cross-covariance))
	      (a (m+ cross-covariance
		     (mult-matrix cross-covariance-t -1)))
	      (delta (make-vect `(,(m-ref a 1 2) ,(m-ref a 2 0) ,(m-ref a 0 1))))

	      ;; (delta (make-matrix 3 1 `((,(aref a 1 2)) (,(aref a 2 0)) (,(aref a 0 1)))))
	      ;; (delta (make-array '(3) :initial-contents `(,(aref a 1 2) ,(aref a 2 0) ,(aref a 0 1))))

	      (q-lower (m+
			(m+ cross-covariance cross-covariance-t)
			(mult-matrix ident (- (matrix-trace cross-covariance)))))
	      (q (make-matrix 4 4)))
	      ;; (q (make-array '(4 4) :element-type 'number :initial-element 0)))
	 (format t "debug~%")
	 (setf (m-ref q 0 0) (matrix-trace cross-covariance))
	 (loop
	    for i from 1 to 3
	    do
	      (setf (m-ref q 0 i) (v-ref delta (- i 1)))
	      (setf (m-ref q i 0) (v-ref delta (- i 1))))
	 (loop
	    for i from 0 to 2
	    do
	      (loop
	 	 for j from 0 to 2
	 	 do
	 	   (setf (m-ref q (+ 1 i) (+ 1 j)) (m-ref q-lower i j))))
	 ;; (format t "pairs: ~A~%" point-correspondences)
	 ;; (format t "current error: ~F~%" current-error)
	 (format t "cross-covariance: ~A~%" cross-covariance)
	 ;; (format t "a: ~A~%" a)
	 ;; (format t "delta: ~A~%" delta)
	 ;; (format t "q-lower: ~A~%" q-lower)
	 (format t "q: ~A~%" q)
	 (let* ((q-matrix (matrix->grid (transpose-matrix q)))
		(rv
		 (multiple-value-bind (eigenvalues eigenvectors)
		     (gsl:eigenvalues-eigenvectors q-matrix)
		   (setf eigenvalues (grid:cl-array eigenvalues))
		   (setf eigenvectors (grid:cl-array eigenvectors))
		   (format t "eigenvalues: ~A~%" eigenvalues)
		   (format t "eigenvectors: ~A~%" eigenvectors)
		   (loop
		      with best-evec = nil
		      with best-eval = nil
		      for e-value across eigenvalues
		      for i upfrom 0
		      if (or (eq nil best-eval)
			     (> (- e-value best-eval) 1e-6))
		      do
			(setf best-evec (loop for j from 0 to 3 collect (aref eigenvectors i j)))
			(setf best-eval e-value)
		      finally (return best-evec))))
		(rq
		 (normalize (make-quaternion (first rv) (second rv) (third rv) (fourth rv))))
		(r-svd
		 (multiple-value-bind (u sigma vt)
		     (gsl:sv-decomposition (matrix->grid cross-covariance))
		   (declare (ignore sigma))
		   (setf u (grid->matrix u))
		   (setf vt (grid->matrix vt))
		   (matrix*matrix (transpose-matrix vt) (transpose-matrix u))))
		(rq-matrix (quaternion->matrix rq))
		(rt
		 (v+ model-centroid-v (v* (matrix*vector (case method
							   (:svd r-svd)
							   (:quaternion rq-matrix))
							 data-centroid-v) -1))))
	   (format t "rq: ~A~%" rq)
	   (format t "rt: ~A~%" rt)
	   (format t "rqm: ~A~%" rq-matrix)
	   (format t "r-svd: ~A~%" r-svd)
	   (format t "rqm-t: ~A~%" (mult-matrix (transpose-matrix rq-matrix) -1))
	   ;; (format t "distance before: ~F~%" (distance (first data-points) (second data-points)))

	   (let* ((next-step-data
		   (loop
		      for p in data-points
		      collect
			(translate rt 
				   (vector->point (matrix*vector (case method
								   (:svd r-svd)
								   (:quaternion rq-matrix))
								 (point->vector p))))))
		  (next-error
		   (loop
		      for pair in point-correspondences
		      for p in next-step-data
		      summing (distance p (second pair)) into error
		      finally (return (/ error num-data-points)))))
	     (format t "previous error: ~F~%" previous-dms-error)
	     (format t "new error: ~F~%" next-error)
	     (if (or (eq nil previous-dms-error)
		     (eq nil dms-error)
		     ;; (> next-error threshold))
		     (> (- previous-dms-error next-error) threshold))
		 (progn
		   (push (list (case method
				 (:svd r-svd)
				 (:quaternion rq-matrix))
			       rt dms-error) retval)
		   (loop
		      for next-p in next-step-data
		      for p in data-points
		      do
			(set-coords p next-p)))
		 (progn
		   (format t "local minimum reached with error ~F~%" next-error)
		   ;; (loop
		   ;;    for next-p in next-step-data
		   ;;    for p in data-points
		   ;;    do
		   ;; 	(set-coords p next-p))
		   (setf stop-flag t))))

	   ;; (format t "transformed data: ~F~%" data-points)
	   ;;(format t "distance after: ~F~%" (distance (first data-points) (second data-points)))
	   (setf previous-dms-error dms-error)
	   (setf dms-error
		 (loop
		    for pair in point-correspondences
		    summing (distance (first pair) (second pair))
		    into error
		    finally (return (/ error num-data-points))))

	   (if (and previous-dms-error dms-error)
	       (format t "error difference: ~F~%" (- previous-dms-error dms-error)))))
	   
     finally (return (reverse retval))))
	      
					
(defun array->grid (a)
  (let* ((dim1 (array-dimension a 0))
	 (dim2 (array-dimension a 1))
	 (result (grid:make-foreign-array 'double-float :dimensions `(,dim1 ,dim2)
					  :initial-contents
					  (loop
					     for i from 0 to (- dim1 1)
					     collect
					       (loop
						  for j from 0 to (- dim2 1)
						  collect
						    (aref a i j))))))
    result))


(defun church-points ()
  (list
   (make-point 0 0 0)
   (make-point 18.47 0 0)
   (make-point 18.47 3.03 0)
   (make-point 21.25 3.03 0)
   (make-point 21.25 6.14 0)
   (make-point 18.47 6.14 0)
   (make-point 18.47 9.17 0)
   (make-point 0 9.17 0)))

(defun test-icp (threshold &optional (method :svd))
  (let* ((model-points (subseq (church-points) 0 4))
	 (q (normalize (make-quaternion 0.642246 0.0 0.0 0.036986)))
	 (rt (make-vect 2.710137 7.101500 0.0))
	 (data-points (subseq (church-points) 0 4)))
    (format t "initial data points: ~A~%" data-points)
    (loop
       for p in data-points
       do
	 (q-rotate* q p)
	 (translate* rt p))
    (format t "data points: ~A~%" data-points)
    (format t "model points: ~A~%" model-points)
    (let ((results (icp data-points model-points :threshold threshold :method method)))
      (format t "data points: ~A~%" results)
      (values results data-points))))
	 

(defun test-multipass-icp (threshold size &optional (method :svd))
  (let* ((model-points (church-points))
	 (q (normalize (make-quaternion 0.642246 0.0 0.0 0.036986)))
	 (rt (make-vect 2.710137 7.101500 0.0))
	 (data-points (subseq (church-points) 0 size)))
    (format t "initial data points: ~A~%" data-points)
    (loop
       for p in data-points
       do
	 (q-rotate* q p)
	 (translate* rt p))
    (format t "data points: ~A~%" data-points)
    (format t "model points: ~A~%" model-points)
    (multiple-value-bind (params err)
	(multipass-icp data-points model-points :threshold threshold :method method)
      (format t "best error was: ~F~%" err)
      (format t "best parameters were: ~F~%" params)
      params)))

(defun create-test-data (size)
   (let* ((q (normalize (make-quaternion 0.642246 0.0 0.0 0.036986)))
	  (rt (make-vect 2.710137 7.101500 0.0))
	  (data-points (subseq (church-points) 0 size)))
     (loop
	for p in data-points
	do
	  (q-rotate* q p)
	  (translate* rt p))
     data-points))

(defun apply-transformation-list (list-of-transformations point-set)
  "list-of-transformations is a list of triplets whose first element is the quaternion
representing the rotation to be applied, and whose second list is the translation vector.
The function applies the transformations in order, first the rotation and then the
translation, to the point set."
  (loop
     for (rq rt) in list-of-transformations
     do
       (loop
	  for p in point-set
	  do
	    (typecase rq
	      (quaternion
	       (q-rotate* rq p))
	      (array
	       (set-coords p (vector->point (matrix*vector rq (point->vector p))))))
	    (translate* rt p))))

(defun apply-inverse-transformation-list (list-of-transformations point-set)
  "The list of transformations is the same as for the forward application. The
only difference is the forward application transforms data to model, whereas this
application transforms model to data."
   (loop
       for (rm rv) in (reverse list-of-transformations)
      ;; we have to inver the transformations to take us from the model
      ;; to the data; another way to do this is to compose all of the 
      ;; transformations first and then invert, which should be more efficient
      ;; but is trickier to write and I am lazy
      do
	(let ((inv-rm 
	       (multiple-value-bind (lu perm flag)
		   (gsl:lu-decomposition (matrix->grid rm))
		 (declare (ignore flag))
		 (grid->matrix (gsl:lu-invert lu perm))))
	      (inv-rv
	       (v* rv -1)))
	  (loop
	     for p in point-set
	     do
	       (translate* inv-rv p)
	       (set-coords p (vector->point (matrix*vector inv-rm (point->vector p))))))))
