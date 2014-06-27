(in-package :geom)

(define-condition constraint-solver-error (error)
  ((text :initarg text)))


(defun create-constraint-graph (objects-and-constraints)
  (let ((g (make-instance 'cl-graph:graph-container)))
    (loop
       for item in objects-and-constraints
       do
	 (add-to-graph item g))
    g))


(defgeneric line-from-point-and-line (ref-point l-ref l-targ cg known-points known-lines))

(defmethod line-from-point-and-line ((ref-point point) 
				     (l-ref line-segment)
				     (l-targ line-segment) 
				     (cg cl-graph:graph-container)
				     (known-points list)
				     (known-lines list))
  (declare (optimize (debug 3)))
  (format t "placing ~A relative to ~A~%" l-targ l-ref)
  (let* ((shared-point
	  (cond
	    ((or (eq (start-point l-ref) (start-point l-targ))
		 (eq (start-point l-ref) (end-point l-targ)))
	     (start-point l-ref))
	    ((or (eq (end-point l-ref) (start-point l-targ))
		 (eq (end-point l-ref) (end-point l-targ)))
	     (end-point l-ref))
	    (t nil)))
	 (free-point
	  (other-end l-targ shared-point))
	 (ref-endpoint
	  (other-end l-ref shared-point))
	 (x1 (point-x shared-point))
	 (y1 (point-y shared-point))
	 (l1 (line-segment-length l-ref))
	 (l2 (line-segment-length l-targ))
	 ;;(x2 (point-x ref-endpoint))
	 ;;(y2 (point-y ref-endpoint))
	 ;;(mr (/ (- y2 y1) (- x2 x1)))
	 ;;(alpha (atan (- y2 y1) (- x2 x1)))
	 (theta-d (if (or (eq nil l-ref)
			  (eq nil l-targ)
			  (not (cl-graph:find-edge-between-vertexes cg l-ref l-targ :error-if-not-found? nil)))
		      90.0
		      (edge->constraint (cl-graph:find-edge-between-vertexes
					 cg l-ref l-targ :error-if-not-found? nil))))
	 ;;(theta (deg->rad theta-d))
	  ;; (cond ((and (> mr 0) (> alpha 0))
	  ;; 	 (deg->rad (- theta-d)))
	  ;; 	((and (< mr 0) (< alpha 0))
	  ;; 	 (deg->rad (- theta-d)))
	  ;; 	((and (< mr 0) (> alpha 0))
	  ;; 	 (deg->rad theta-d))
	  ;; 	((and (> mr 0) (< alpha 0))
	  ;; 	 (deg->rad theta-d))
	  ;; 	(t (deg->rad theta-d))))
	 (target-point
	  ;; (rotate-point-2d ref-endpoint (deg->rad theta-d) shared-point)

	  (let* ((orientation1
	  	  (let* ((pm (rotate-point-2d ref-endpoint (deg->rad theta-d) shared-point))
	  		 (xm (point-x pm))
	  		 (ym (point-y pm))
	  		 (xd (* (/ l2 l1) (- xm x1)))
	  		 (yd (* (/ l2 l1) (- ym y1))))
	  	    (make-point (+ x1 xd) (+ y1 yd))))
		 (orientation2
		  (let* ((pm (rotate-point-2d ref-endpoint (deg->rad (- theta-d)) shared-point))
	  		 (xm (point-x pm))
	  		 (ym (point-y pm))
	  		 (xd (* (/ l2 l1) (- xm x1)))
	  		 (yd (* (/ l2 l1) (- ym y1))))
	  	    (make-point (+ x1 xd) (+ y1 yd))))
	    	 (min-dist-to-known (min-distance ref-point known-points))
		 (max-dist-to-known (max-distance ref-point known-points))
	  	 (dist-to-o1 (distance-2d ref-point orientation1))
		 (dist-to-o2 (distance-2d ref-point orientation2)))

	    (format t "max-dist-to-known: ~2,2F, min-dist-to-known: ~2,2F~%" max-dist-to-known min-dist-to-known)
	    (format t "rotating ~A by ~2,2F around ~A produced ~A~%" 
		    ref-endpoint theta-d shared-point orientation1)
	    (format t "rotating ~A by ~2,2F around ~A produced ~A~%" 
		    ref-endpoint (- theta-d) shared-point orientation2)
	    
	    (cond ((and (is-point-visible? orientation1 ref-point known-lines)
			(is-point-visible? orientation2 ref-point known-lines))
		   (progn
		     (format t "both ~A and ~A visible from ~A, using ~A~%"
			     orientation1 orientation2 ref-point orientation1)
		     orientation1))
		     ;;(rotate-point-2d ref-endpoint (deg->rad theta-d) shared-point)))
		  ((and (is-point-visible? orientation1 ref-point known-lines)
			(not (is-point-visible? orientation2 ref-point known-lines)))
		   (progn
		     (format t "~A is visible from ~A but ~A is not, using ~A~%"
			     orientation1 ref-point orientation2 orientation2)
		     orientation2))
		     ;;(rotate-point-2d ref-endpoint (deg->rad (- theta-d)) shared-point)))
		  ((and (is-point-visible? orientation2 ref-point known-lines)
			(not (is-point-visible? orientation1 ref-point known-lines)))
		   (progn
		     (format t "~A is visible from ~A but ~A is not, using ~A~%"
			     orientation2 ref-point orientation1 orientation1)
		     orientation1))
		     ;;(rotate-point-2d ref-endpoint (deg->rad theta-d) shared-point)))
		  ((and (not (is-point-visible? orientation2 ref-point known-lines))
			(not (is-point-visible? orientation1 ref-point known-lines)))
		   (progn
		     (format t "neither ~A nor ~A visible from ~A, moving on to geom constraints~%"
			     orientation1 orientation2 ref-point)
		     ;; this bit doesn't quite work; I suspect we need to pick the orientation
		     ;; that maximizes? minimizes? the difference between the distance to that point and
		     ;; the distance to the farthest known point

		     orientation1))

		     ;; (cond ((and (< dist-to-o1 max-dist-to-known)
		     ;; 		 (> dist-to-o2 
		     ;; 	    (progn
		     ;; 	      (format t "~A is too close (~2,2F) to already-known points, using ~A~%"
		     ;; 		      orientation1 dist-to-o1 orientation2)
		     ;; 	      orientation2))
		     ;; 	      ;;(rotate-point-2d ref-endpoint (deg->rad (- theta-d)) shared-point)))
		     ;; 	   ((< dist-to-o2 max-dist-to-known)
		     ;; 	    (progn
		     ;; 	      (format t "~A is too close (~2,2F) to already-known points, using ~A~%"
		     ;; 		      orientation2 dist-to-o2 orientation1)
		     ;; 	      orientation1))
		     ;; 	      ;;(rotate-point-2d ref-endpoint (deg->rad theta-d) shared-point)))

		     ;; 	   (t orientation1))))

		  (t orientation1)))))
    (format t "target point result: ~A~%" target-point)
    (format t "shared point: ~A~%ref-endpoint: ~A~%free point: ~A~%" shared-point ref-endpoint free-point)
    ;;(format t "theta: ~2,2F, xm: ~2,2F, ym: ~2,2F, x1: ~2,2F, y1: ~2,2F, l1: ~2,2F, l2: ~2,2F, xd: ~2,2F, yd: ~2,2F, x3: ~2,2F, y3: ~2,2F~%" (rad->deg theta) xm ym x1 y1 l1 l2 xd yd x3 y3)
    ;;(format t "~{~2,2F~^ ~}~%" `(,(rad->deg theta) ,xm ,ym ,l1 ,l2 ,xd ,yd ,x3 ,y3))
    (set-coords free-point `(,(cons :x (point-x target-point)) 
			     ,(cons :y (point-y target-point))))
    (format t "l-targ: ~A~%~%~%" l-targ)))
	
(defgeneric place-geometry (cg reference-coords)
  (:documentation "Place the geometry given the constraint graph."))
     

(defmethod place-geometry ((cg cl-graph:graph-container) (reference-coords point))
  "Places the geometry of the given constraint graph. If something can't be placed, we should probably exit gracefully and do our best, but in fact we're just going to give up instead."
  (declare (optimize (debug 3)))
  (let* ((known-lines (cl-graph:find-vertexes-if cg #'(lambda (x)
							(and
							 (is-geom-valid? (node->geometry x))
							 (eq (type-of (node->geometry x)) 'line-segment)))))
	 (known-points
	  (loop
	     for ls in known-lines
	     append
	       (list (start-point (node->geometry ls)) (end-point (node->geometry ls))))))
    (format t "known lines: ~A~%" known-lines)
    (format t "known points: ~A~%" known-points)
    (format t "known distances: ~A~%" (mapcar #'third (all-distances reference-coords known-points)))
    (loop named geometry-placer
       ;; find all the nodes remaining to be placed
       ;; these are nodes that contain either points or line segments
       ;; and do not have a valid geometry
       with valid-nodes = (cl-graph:find-vertexes-if 
			   cg
			   #'(lambda (x)
			       (not (is-geom-valid? (node->geometry x)))))
       with node-to-fix = '()
       with node1 = '()
       with node2 = '()
       ;; note: not really proper CLOS
       ;; TODO: add is-geom-valid? that can handle vertex containers
       while (> (length valid-nodes) 0)
       do
       ;;(format t "valid-nodes: ~A~%" valid-nodes)
	 (loop named candidate-finder
	    for i upfrom 1
	    for candidate-node in valid-nodes
	    do
	    ;; (format t "candidate node ~D: ~A~%" i candidate-node)
	    ;; 1. get all the neighbors of the candidate who have valid geometry
	    ;; 2. find all of those neighbors who are connected to each other
	      (let* ((fixed-neighbors 
		      (cl-graph:neighbor-vertexes candidate-node 
						  #'(lambda (x)
						      (is-geom-valid? (node->geometry x)))))
		     (connected-fixed-neighbors
		      (loop
			 for n1 in fixed-neighbors
			 append
			   (loop
			      for n2 in fixed-neighbors
			      ;; do
			      ;;   (format t "~A and ~A connected? ~A~%" n1 n2
			      ;;       (cl-graph:find-edge-between-vertexes cg n1 n2 :error-if-not-found? nil))
			      when (cl-graph:find-edge-between-vertexes cg n1 n2 :error-if-not-found? nil)
			      append (list (node->geometry n1) (node->geometry n2))))))
		;; (format t "fixed-neighbors: ~A~%" fixed-neighbors)
		;; (format t "connected-fixed-neighbors: ~A~%" connected-fixed-neighbors)
		(when (>= (length connected-fixed-neighbors) 2)
		  (setf node-to-fix (node->geometry candidate-node))
		  (setf node1 (first connected-fixed-neighbors))
		  (setf node2 (second connected-fixed-neighbors))
		  ;; (format t "set candidate node~%")
		  (return-from candidate-finder))))
       ;; (format t "node-to-fix: ~A, type:~A~%" node-to-fix (type-of node-to-fix))
       ;; (format t "node1: ~A, type: ~A~%" node1 (type-of node1))
       ;; (format t "node2: ~A, type: ~A~%" node2 (type-of node2))
       ;; this cond determines how we fix the geometry
	 (cond 
	   ;; not too informative, but we should never be trying to constrain
	   ;; a free-floating point relative to two other points
	   ;; this represents a disaster so we should exit immediately     
	   ((and (eq (type-of node-to-fix) 'point)
		 (eq (type-of node1) 'point)
		 (eq (type-of node2) 'point))
	    (error 'constraint-solver-error :text "Attempting to constrain free-floating point!"))
	   ;; line from point and line
	   ;; note: does not actually use point
	   ;; two identical cases with argument orders switched
	   ((and (eq (type-of node-to-fix) 'line-segment)
		 (eq (type-of node1) 'point)
		 (eq (type-of node2) 'line-segment))
	    (line-from-point-and-line reference-coords node2 node-to-fix cg known-points known-lines))
	   ((and (eq (type-of node-to-fix) 'line-segment)
		 (eq (type-of node1) 'line-segment)
		 (eq (type-of node2) 'point))
	    (line-from-point-and-line reference-coords node1 node-to-fix cg known-points known-lines))
	   ;; now the case where a line is constrained by two endpoints
	   ;; then just set the endpoints of the line
	   ((and (eq (type-of node-to-fix) 'line-segment)
		 (eq (type-of node1) 'point)
		 (eq (type-of node2) 'point))
	    (set-endpoints node-to-fix node1 node2))
	   ;; now the two cases where a point is constrained by a line segment
	   ;; and another endpoint
	   ;; two identical cases, with orders switched
	   ((and (eq (type-of node-to-fix) 'point)
		 (eq (type-of node1) 'point)
		 (eq (type-of node2) 'line-segment))
	    (let* ((x1 (point-x node1))
		   (m (line-segment-m node2))
		   (b (line-segment-b node2))
		   (d (line-segment-length node2))
		   (x2 (/ (+ x1 d) (sqrt (+ 1 (expt m 2)))))
		   (y2 (+ (* m x2) b)))
	      (format t "~A~%" `(,(cons :x x2) ,(cons :y y2)))
	      (set-coords node-to-fix `(,(cons :x x2) ,(cons :y y2)))))
	   ((and (eq (type-of node-to-fix) 'point)
		 (eq (type-of node1) 'line-segment)
		 (eq (type-of node2) 'point))
	    (let* ((x1 (point-x node2))
		   (m (line-segment-m node1))
		   (b (line-segment-b node1))
		   (d (line-segment-length node1))
		   (x2 (/ (+ x1 d) (sqrt (+ 1 (expt m 2)))))
		   (y2 (+ (* m x2) b)))
	      (format t "~A~%" `(,(cons :x x2) ,(cons :y y2)))
	      (set-coords node-to-fix `(,(cons :x x2) ,(cons :y y2))))))
	 (setf known-lines (cl-graph:find-vertexes-if cg #'(lambda (x)
							     (and
							      (is-geom-valid? (node->geometry x))
							      (eq (type-of (node->geometry x)) 'line-segment)))))
	 (let ((remaining-nodes  (cl-graph:find-vertexes-if 
				  cg
				  #'(lambda (x)
				      (not (is-geom-valid? (node->geometry x)))))))
	   ;; if no nodes have been fixed during the previous iteration
	   ;; then there's nothing left to do. possibly the problem is not
	   ;; well-constrained, but in any case, we should stop
	   ;; (format t "remaining-nodes: ~A~%" remaining-nodes)
	   (if (= (length remaining-nodes) (length valid-nodes))
	       (return-from geometry-placer)
	       (setf valid-nodes remaining-nodes))))))


	  ;; (let ((saved-ls (copy-geometry node-to-fix))
	  ;; 	(other-walls (cl-graph:find-vertexes-if cg #'(lambda (x)
	  ;; 						       (and (is-geom-valid? x)
	  ;; 							    (eq (type-of x) 'line-segment))))))
		 
	  ;;   (line-from-point-and-line node1 node2 node-to-fix cg)
	  ;;   ;; check if we've done something terrible like generated intersecting walls
	  ;;   (when (every #'(lambda (x)
	  ;; 		     (not (intersect? node-to-fix x)))
	  ;; 		 other-walls)
	  ;;     ;; oh no! we have generated a bad wall
	  ;;     ;; we should revert


 ;; (xd (* (/ l2 l1) (cond ((and (< xm 0)
 ;; 				      (< x1 0))
 ;; 				 (- xm x1))
 ;; 				((and (> xm 0)
 ;; 				      (> x1 0))
 ;; 				 (- xm x1))
 ;; 				((or (and (> xm 0)
 ;; 					  (< x1 0))
 ;; 				     (and (< xm 0)
 ;; 					  (> x1 0)))
 ;; 				 (abs (- (abs xm) (abs x1))))
 ;; 				(t (- xm x1)))))
				     
 ;; 	 (yd (* (/ l2 l1) (cond ((and (< ym 0)
 ;; 				      (< y1 0))
 ;; 				 (- ym y1))
 ;; 				((and (> ym 0)
 ;; 				      (> y1 0))
 ;; 				 (- ym y1))
 ;; 				((or (and (> ym 0)
 ;; 					  (< y1 0))
 ;; 				     (and (< ym 0)
 ;; 					  (> y1 0)))
 ;; 				 (abs (- (abs ym) (abs y1))))
 ;; 				(t (- ym y1)))))



(defun is-point-visible? (p robot-coords visible-segments)
    "A function that answers the question, would the point in question be
visible to the robot given the other line segments that it already sees? This
is answered by drawing a ray from the robot to the point and seeing if it intersects
any of the visible line segments."
  (declare (type point p robot-coords))
  (declare (type list visible-segments))
  (let ((view-ls (make-line (make-point (point-x p) (point-y p) 0.0)
			    (make-point (point-x robot-coords) (point-y robot-coords) 0.0))))
    (loop
       for visible-ls in visible-segments
       if (intersect? view-ls (node->geometry visible-ls))
       do
	 (return nil)
       finally (return t))))

;; (remove-method #'constraint-solver::line-from-point-and-line (find-method #'constraint-solver::line-from-point-and-line ()
;; 										   '(constraint-solver::point constraint-solver::line-segment constraint-solver::line-segment
;; 										     cl-graph:graph-container list)))

(defun position-schema (model observed)
  "Updates OBSERVED to have all its component segments geometrically valid, and matching as closely
   as practical the shape of those in MODEL. Both arguments are constraint-schemas. All the segments
   in MODEL should be geometrically valid, though typically one or more in OBSERVED are not. Both
   arguments should have the same number of segments, arranged in the same order. The constraints in
   both arguments are ignored. Returns the updated OBSERVED."
  (multiple-value-bind (start length) (find-candidate-run observed)
    (let* ((model-centroid (interior-centroid model start length))
           (translation (difference (interior-centroid observed start length) model-centroid))
           (translated (translated-schema model (point-x translation) (point-y translation)))
           (translated-centroid (make-point (+ (point-x model-centroid) (point-x translation))
                                            (+ (point-y model-centroid) (point-y translation)))))
      (multiple-value-bind (angle sum-of-squares) (best-rotation translated translated-centroid observed start length)
	(format t "best sum of squares was ~F, best angle was ~F~%" sum-of-squares angle)
	(rotate-object translated angle translated-centroid)
	(loop
	   for model-ls in (constraint-schema-objects translated)
	   for observed-ls in (constraint-schema-objects observed)
	   do
	     (set-endpoint-coords observed-ls
				  (start-point model-ls)
				  (end-point model-ls)))))))
	     

(defun position-schema-global-min (model observed)
  "Does the same as above, but exhaustively searches the space rather than settling on local minima."
  (multiple-value-bind (start length)
      (find-candidate-run observed)
    (let* ((model-centroid (interior-centroid model start length))
	   (translation (difference (interior-centroid observed start length) model-centroid))
	   (translated (translated-schema model (point-x translation) (point-y translation)))
	   (translated-centroid
	    (make-point (+ (point-x model-centroid) (point-x translation))
			(+ (point-y model-centroid) (point-y translation))))
	   ;;(theta-range (make-linear-vector #'(lambda (x) x) 0 (- (* 2 pi) 0.001) 100))
	   (theta-start 0)
	   (theta-end (- (* 2 pi) 0.001))
	   (sums-of-squares (make-linear-vector 
			     #'(lambda (x)
				 (sum-of-squares-of-distances model 
							      translated-centroid 
							      x observed start length))
			     theta-start theta-end 100)))
      (multiple-value-bind (min-index theta-min sos-min)
	  (minimize-function sums-of-squares theta-start theta-end 100)
	(format t "best rotation is: ~F, has sum-of-squares: ~F, index: ~F~%" theta-min sos-min min-index)
	(loop for model-segment in (constraint-schema-objects translated)
	   for observed-segment in (constraint-schema-objects observed)
	   for start = (start-point observed-segment)
	   for end = (end-point observed-segment)
	   for update = nil
	   unless (is-geom-valid? start)
	   do 
	     (setf start (rotate-point-2d (start-point model-segment) theta-min translated-centroid)
		   update t)
	   unless (is-geom-valid? end)
	   do 
	     (setf end (rotate-point-2d (end-point model-segment) theta-min translated-centroid) 
		   update t)
	   when update
	   do (set-endpoints observed-segment start end))))))
      

;; (defun make-test-schema (segs)
;;   (labels ((f (n) (and n (float n))))
;;     (loop for (x0 y0 x1 y1) in segs
;;           collect (make-line (make-point (f x0) (f y0)) (make-point (f x1) (f y1)) 0.0) into segments
;;           finally (return (make-constraint-schema segments nil)))))
;;
;; (defun test-position-schema (model-data observed-data)
;;   (let ((observed-schema (make-test-schema observed-data)))
;;     (values (position-schema (make-test-schema model-data) observed-schema)
;;             observed-schema)))
;; 
;; (test-position-schema '((0 0 1 1) (1 1 2 0) (2 0 -1 -1) (-1 -1 0 0))
;;                       '((10 10 11 11) (11 11 12 10) (12 10 nil nil) (nil nil 10 10)))
;; 
;; (test-position-schema '((0 0 1 1) (1 1 2 0) (2 0 -1 -1) (-1 -1 0 0))
;;                       '((0 0 nil nil) (nil nil nil nil) (nil nil -1 -1) (-1 -1 0 0)))
;; 
;; (test-position-schema '((0 0 1 1) (1 1 2 0) (2 0 -1 -1) (-1 -1 0 0))
;;                       '((0 0 nil nil) (nil nil nil nil) (nil nil -1 -1) (-1 -1 0 0)))
;; 
;; (test-position-schema '((0 8 5 13) (5 13 6 12) (6 12 7 13) (7 13 8 12) ; church
;;                         (8 12 7 11) (7 11 8 10) (8 10 3 5) (3 5 0 8))
;;                       '(nil nil (nil nil 10 5) (10 5 9 4) (9 4 8 5) (8 5 7 4) (7 4 nil nil) nil))
;; 
;; (test-position-schema '((11 9 6 4) (6 4 1 9) (1 9 6 14) (6 14 11 9)) ; hotel
;;                       '((9 4 2 3) (2 3 0 9.7823) (0 9.7823 nil nil) (nil nil 9 4)))
;; 
;; (find-runs (make-test-schema '((1 2 3 4) (3 4 5 6) (5 6 7 8) (7 8 nil nil) nil nil (nil nil 1 2))))


(defun find-candidate-run (schema)
  "Finds the best run of contiguous segments in SCHEMA for mapping a model schema to
   an observed schema. This is the longest continuous run of segements in SCHEMA,
   or an arbitrary one of them if there are more than one such runs. If such a
   run exists, it returns two values, the zero-based index in the list of segments in
   SCHEMA of the start of that run, and the length of that run. Note that a run may
   wrap around."
  (let ((runs (find-runs schema)))
    (format t "runs: ~A~%" runs)
    (apply #'values (find (reduce #'max runs :key #'second) runs :key #'second))))

(defun find-runs (schema)
  (let ((segments (constraint-schema-objects schema)) (runs '()))
    (loop with current-start = nil
          for index from 0
          for segment in segments
       do 
	 (format t "segment: ~A, geom valid? ~A~%" segment (is-geom-valid? segment))
       do (cond ((not (is-geom-valid? segment))
		 (when current-start
		   (push (list current-start (- index current-start)) runs)
		   (setf current-start nil)))
		((null current-start)
		 (setf current-start index)))
       finally (when current-start
		 (push (list current-start (- index current-start)) runs)))
    (format t "runs: ~A~%" runs)
    (let ((last-run (first (last runs))))
      (when (and (= (apply #'+ (first runs)) (length segments))
                 (zerop (first last-run))
		 (> (length runs) 1))
        ;; wraps around
        (incf (second (first runs)) (second last-run))
	(format t "runs: ~A~%" runs)
        (setf runs (nbutlast runs))
	(format t "runs: ~A~%" runs)
	))
    runs))

(defun segments-in-run (schema start length)
  (let* ((segments (constraint-schema-objects schema))
         (fragment (nthcdr start segments))
         (prefix-length (length fragment)))
    (if (> length prefix-length)
        (append fragment (subseq segments 0 (- length prefix-length)))
        (subseq fragment 0 length))))

(defun interior-centroid (schema start length)
  "Computes the centroid of the interior points of the run of segments in SCHEMA starting
   at START and of length LENGTH. It omits the start point of the first segment of that run
   and the end point of the last segment of that run. Note that a run can wrap around. Returns
   a point."
  (declare (optimize (debug 3)))
  (let ((candidate (segments-in-run schema start length)))
    (loop with first = (end-point (pop candidate))
          for seg in candidate
          count t into n
          sum (point-x (start-point seg)) into x-sum
          sum (point-y (start-point seg)) into y-sum
          finally (return (make-point (/ (+ x-sum (point-x first)) (+ n 1))
                                      (/ (+ y-sum (point-y first)) (+ n 1)))))))

(defun translated-schema (schema delta-x delta-y)
  "Returns a new constraint-schema that has the same geometry as SCHEMA, but has
   been translated. All the segments in SCHEMA must be geometrically valid. The
   result constains no constraints."
  (make-constraint-schema (loop for segment in (constraint-schema-objects schema)
                                collect (make-line (make-point (+ (point-x (start-point segment)) delta-x)
                                                               (+ (point-y (start-point segment)) delta-y))
                                                   (make-point (+ (point-x (end-point segment)) delta-x)
                                                               (+ (point-y (end-point segment)) delta-y))
                                                   0.0))
                          nil))

(defconstant +sample-angle+ (/ pi 3))
(defconstant +best-rotation-tolerance+ 0.01)

(defun best-rotation (model centroid observed start length)
  (labels ((f (theta)
             (sum-of-squares-of-distances model centroid theta observed start length)))
    (let ((seed nil))
      ;; sample around the circle to find a good starting point
      (loop with minimum-found = nil
            for theta = (- (* 2 pi) +sample-angle+) then (- theta +sample-angle+)
            while (>= theta -0.0001) ; we might miss zero otherwise 'cause of FP rounding
            for delta = (f theta)
            if (or (null seed) (< delta minimum-found))
            do (setf seed theta minimum-found delta))
      (let ((result 
	     ;;(find-minimum #'f (- seed +sample-angle+) seed (+ seed +sample-angle+) +best-rotation-tolerance+)))
	     (find-minimum #'f 0.0 (- (* 2 pi) 0.01) (+ seed +sample-angle+) +best-rotation-tolerance+)))
        (values result (f result))))))

(defun sum-of-squares-of-distances (model centroid theta observed start length)
  (labels ((square-of-distance (model-point observed-point)
             (expt (distance-2d (rotate-point-2d model-point theta centroid)
                                observed-point)
                   2)))
    ;; TODO it's kind of silly to keep consing the same darn thing every time we call this
    (loop for model-segment in (segments-in-run model start length)
          for observed-segment in (segments-in-run observed start length)
          sum (square-of-distance (start-point model-segment) (start-point observed-segment))
          into result
          finally (return (+ result (square-of-distance (end-point model-segment) (end-point observed-segment)))))))

(defconstant +2-golden-ratio+ (- 2 (/ (+ 1 (sqrt 5)) 2)))

(defun find-minimum (function start intermediate end tolerance)
  "Numerically finds the minimum of a function of one variable using golden ratio search.
   FUNCTION should be a real valued function of one real variable. START, INTERMEDIATE and
   END are all reals, such that START < INTERMEDIATE < END, and such that the value of
   FUNCTION called on INTERMEDIATE is less than it called on either START or END. FUNTION
   should be strictly unimodal between START and END. TOLERANCE is the tightness of the
   interval for which a solution is sought. Returns a real, the relevant value passed to
   FUNCTION to produce its minimum value."  
  (let ((cache (make-hash-table)))
    (labels ((cached-function (x)
               ;; Doesn't seem to be any reason to go to the trouble of zapping no longer
               ;; reachable values from the cache, but if called with a huge range and
               ;; tiny tolerance such an optimization might become attractive; of course,
               ;; in that case we should be using a completely different algorithm.
               (or (gethash x cache nil) (setf (gethash x cache) (funcall function x))))
             (golden-section-search (s i e)
               (let ((probe (if (> (- e i) (- i s))
                              (+ i (* +2-golden-ratio+ (- e i)))
                              (- i (* +2-golden-ratio+ (- i s))))))
                 (cond ((< (abs (- e s)) (* tolerance (+ (abs i) (abs probe))))
                        (/ (+ s e) 2))
                       ((<= (cached-function probe) (cached-function i))
                        (if (> (- e i) (- i s))
                          (golden-section-search i probe e)
                          (golden-section-search s probe i)))
                       (t
                        (if (> (- e i) (- i s))
                          (golden-section-search s i probe)
                          (golden-section-search probe i e)))))))
      (golden-section-search start intermediate end))))
