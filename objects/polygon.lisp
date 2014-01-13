(in-package :geom)

(defclass constraint ()
  ((obj1 :accessor constraint-obj1
	 :initform nil
	 :initarg :obj1)
   (obj2 :accessor constraint-obj2
	 :initform nil
	 :initarg :obj2)
   (constraint-value :accessor constraint-value
		     :initform nil
		     :initarg :value)
   (constraint-type :accessor constraint-type
		    :initform nil
		    :initarg :type)))

(defmethod add-to-graph ((c constraint) (g cl-graph:graph-container))
  (cl-graph:add-edge-between-vertexes
   g (constraint-obj1 c) (constraint-obj2 c) :value (constraint-value c)))

(defmethod make-constraint ((obj1 geometric-object) (obj2 geometric-object)
			    (value float) (constraint-type symbol))
  (make-instance 'constraint :obj1 obj1 :obj2 obj2 :value value :type constraint-type))

(defclass constraint-schema ()
  ((objects :accessor constraint-schema-objects
	    :initform nil
	    :initarg :objects)
   (constraints :accessor constraint-schema-constraints
		:initform nil
		:initarg :constraints)))

(defmethod constraint-schema->graph ((cs constraint-schema))
  (let ((cg (cl-graph:make-graph 'cl-graph:graph-container)))
    (loop
       for object in (constraint-schema-objects cs)
       do
	 (add-to-graph object cg))
    (loop
       for constraint in (constraint-schema-constraints cs)
       do
	 (add-to-graph constraint cg))
    cg))

(defmethod geometry->stl ((cs constraint-schema) &optional (terminator t))
  (let ((line-segments (constraint-schema-objects cs)))
    (format nil  "~:[~;[~] ~{~A~^, ~} ~:[~;]~]"
	    terminator
	    (loop
	       for ls in line-segments
	       collect
		 (geometry->stl ls nil))
	    terminator)))

(defun make-constraint-schema (objects constraints)
  "Nice wrapper for constraint-schema"
  (make-instance 'constraint-schema :objects objects :constraints constraints))

(defmethod set-height ((cs constraint-schema) (value number))
  (loop
     for obj in (constraint-schema-objects cs)
     do
       (set-height obj value)))

(defmethod reset-schema ((cs constraint-schema))
  (loop
     for ls in (constraint-schema-objects cs)
     do
       (set-coords (start-point ls) nil)
       (set-coords (end-point ls) nil)
       (setf (line-segment-nx ls) nil)
       (setf (line-segment-ny ls) nil)))

(defmethod print-object ((cs constraint-schema) stream)
  (format stream "~{~A~^~%~}~%" (constraint-schema-objects cs)))

(defmethod geometry->points ((cs constraint-schema))
  (loop
     for obj in (constraint-schema-objects cs)
     append
       (geometry->points obj)))

(defun build-schema-from-list (lines constraint-pairs)
  "lines is a list of two-item lists whose first item is the name of the line segment
and whose second item is the length. constraints is a list of three-item lists whose 
first and second items are the names of the two line segments and whose third item
is the angle between them.
e.g.: ((l1 10.0) (l2 15.0)) and ((l1 l2 90.0))"
  (let ((segment-table (make-hash-table)))
    ;; build a hash table of the line-segment objects using their name as the key
    (let* ((objects
	    (loop
	       for item in lines
	       collect
		 (let* ((key (first item))
			(l (second item))
			(new-line (make-line (make-point) (make-point) l)))
		   (setf (gethash key segment-table) new-line)
		   new-line)))
	   ;; set the constraints - keys supplied in the constraints must match those supplied
	   ;; in lines or terrible things will happen
	   (constraints
	    (loop
	       for item in constraint-pairs
	       collect
		 (let ((ls1 (gethash (first item) segment-table))
		       (ls2 (gethash (second item) segment-table)))
		   (make-constraint ls1 ls2 (third item) :angle)))))
      (make-constraint-schema objects constraints))))

(defmethod centroid ((cs constraint-schema))
  (centroid (constraint-schema-objects cs)))

(defmethod rotate-object ((cs constraint-schema) (angle number) (ref-point point))
  (rotate-objects (constraint-schema-objects cs) angle ref-point))

(defmethod dump-object ((cs constraint-schema) (output-file string))
  (with-open-file (stream output-file :if-exists :supersede :direction :output)
    (format stream "~{~A~^,~}~%" '(x-start y-start x-end y-end))
    (loop
       for ls in (constraint-schema-objects cs)
       do
	 (format stream "~{~,2F~^,~}~%" (list (point-x (start-point ls))
					      (point-y (start-point ls))
					      (point-x (end-point ls))
					      (point-y (end-point ls)))))))

(defmethod reflect-object ((cs constraint-schema) (ref-line line-segment))
  (reflect-objects (constraint-schema-objects cs) ref-line))
