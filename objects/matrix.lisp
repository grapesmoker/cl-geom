(in-package :geom)

;; General `nice' matrix class that does things like check validity for you before
;; you use the matrix and provides lots of nice functionality 

;; Are matrices really geometric objects? Is anything, like, real, man?!

(defclass matrix (geometric-object)
  ;; the underlying array representation
  ((matrix-array :initarg :array
		 :accessor matrix-array)))

;; We subclass matrix generally for a few specific purposes. Of course arbitrary matrices
;; can be any size, but matrices specifically for geometric use are either going to be
;; 3x3 or 4x4.

(defclass mat3 (matrix)
  ())

(defclass mat4 (matrix)
  ())

(defun list-of-lists-of-numbers? (thing)
  "Check to see if thing is a list of lists of numbers."
  (if (listp thing)
      (and (every #'listp thing)
	   (every #'(lambda (x)
		      (every #'numberp x))
		  thing))
      nil))

(defun array->list (a)
  "Convert a 2D array into a list."
  (loop
     for i below (array-dimension a 0)
     collect
       (loop
	  for j below (array-dimension a 1)
	  collect (aref a i j))))

(defun make-matrix (rows columns &optional content)
  (cond ((and (not (null content))
	      (list-of-lists-of-numbers? content)
	      (not (every #'(lambda (x) (= columns (length x))) content)))
	 (error 'invalid-matrices :geometry-object content :error-text "Content has unequal columns!"))
	((not (and (numberp rows)
		   (numberp columns)
		   (or (null content)
		       (list-of-lists-of-numbers? content))))
	 (error 'invalid-matrices :geometry-object content :error-text "Invalid content or rows/columns specified!"))
	((and (numberp rows)
	      (numberp columns)
	      (null content))
	 (setf content (loop
			  for i below rows
			  collect
			    (loop
			       for j below columns
			       collect 0)))
	 (make-instance 'matrix :array (make-array `(,rows ,columns) :element-type 'number :initial-contents content)))
	(t
	 (make-instance 'matrix :array (make-array `(,rows ,columns) :element-type 'number :initial-contents content)))))

(defun ident-matrix (size)
  (make-matrix size size (loop
			    for i below size
			    collect
			      (loop
				 for j below size
				 collect
				   (if (= i j) 1 0)))))

(defmethod m-ref ((m matrix) (i fixnum) (j fixnum))
  "Accessor function for matrices."
  (aref (matrix-array m) i j))

(defmethod update-m-ref ((m matrix) (i fixnum) (j fixnum) (new-val number))
  (setf (aref (matrix-array m) i j) new-val))

(defsetf m-ref update-m-ref)

(defmethod matrix-rows ((m matrix))
  (array-dimension (matrix-array m) 0))

(defmethod matrix-cols ((m matrix))
  (array-dimension (matrix-array m) 1))

(defmethod print-object ((m matrix) stream)
  (format stream "<#MATRIX ~A>" (matrix-array m)))

(defmacro with-valid-matrices ((matrices) &rest body)
  (let ((g1 (gensym))
	(g2 (gensym))
	(valid-flag (gensym)))
  `(progn
     (let ((,valid-flag
	    (loop 
	       for m in ,matrices
	       do
		 (let ((,g1 (matrix-rows m))
		       (,g2 (matrix-cols m)))
		   (loop
		      for i below ,g1
		      do
			(loop
			   for j below ,g2
			   if (not (numberp (m-ref m i j)))
			   do (return nil))))
	       finally (return t))))
       (if (not ,valid-flag)
	   (error 'invalid-matrices :geometry-object ,matrices :error-text "Invalid matrices!")
	   (progn
	     ,@body))))))

(defun m-foo (m1 m2)
  (with-valid-matrices ((list m1 m2))
    (let* ((dim1 (array-dimension m1 0))
	   (dim2 (array-dimension m1 1))
	   (result (make-array `(,dim1 ,dim2) :element-type 'number)))
      )))

(defun m+ (m1 m2)
  (with-valid-matrices ((list m1 m2))
    (let* ((dim1 (matrix-rows m1))
	   (dim2 (matrix-cols m1))
	   (result (make-matrix dim1 dim2)))
      (print 'foo)
      (loop
	 for i below dim1
	 do
	   (loop
	      for j below dim2
	      do
		(setf (m-ref result i j) (+ (m-ref m1 i j) (m-ref m2 i j)))))
      result)))

(defmethod tensor-product ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (let* ((s (vec-size v1))
	     (matrix (make-matrix s s)))
	(loop
	   for i below s
	   do
	     (loop
		for j below s
		do
		  (setf (m-ref matrix i j)
			(* (v-ref v1 i) (v-ref v2 j)))))
	matrix))))

#|
(defmethod tensor-product ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (let* ((matrix (make-matrix 3 3))
	     (v1-contents (components v1))
	     (v2-contents (components v2)))
	(loop
	   for i from 0 to 2
	   for v1-elem in v1-contents
	   do
	     (loop
		for j from 0 to 2
		for v2-elem in v2-contents
		do
		  (setf (m-ref matrix i j) (* v1-elem v2-elem))))
	matrix))))
|#

(defun mult-matrix (m s)
  (let* ((dim1 (matrix-rows m))
	 (dim2 (matrix-cols m))
	 (result (make-matrix dim1 dim2)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (m-ref result i j) (* (m-ref m i j) s))))
    result))

(defun transpose-matrix (m)
    (let* ((dim1 (matrix-rows m))
	   (dim2 (matrix-cols m))
	   (result (make-matrix dim1 dim2)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (m-ref result i j) (m-ref m j i))))
    result))

(defun matrix-trace (m)
  (let* ((dim1 (matrix-rows m))
	 (dim2 (matrix-cols m)))
    (assert (= dim1 dim2))
    (loop
       for i from 0 to (- dim1 1)
       summing (m-ref m i i) into result
       finally (return result))))

(defmethod matrix*vector ((m matrix) (v vect))
  (assert (= (matrix-cols m) (vec-size v)))
  (make-vect 
   (loop
      for i below (matrix-rows m)
      collect
	(loop
	   for j below (matrix-cols m)
	   summing (* (m-ref m i j) (v-ref v j))))))

;; (defun matrix*vector (m v)
;;   ;; vectors are always size 3
;;   (assert (= (array-dimension m 1) 3))
;;   (let ((x (vect-x v)) (y (vect-y v)) (z (vect-z v)))
;;     (make-vect (+ (* (aref m 0 0) x)
;; 		  (* (aref m 0 1) y)
;; 		  (* (aref m 0 2) z))
;; 	       (+ (* (aref m 1 0) x)
;; 		  (* (aref m 1 1) y)
;; 		  (* (aref m 1 2) z))
;; 	       (+ (* (aref m 2 0) x)
;; 		  (* (aref m 2 1) y)
;; 		  (* (aref m 2 2) z)))))

    

(defun matrix*matrix (m1 m2)
  (assert (= (matrix-cols m1)
	     (matrix-rows m2)))
  (let* ((rdim1 (matrix-rows m1))
	 (rdim2 (matrix-cols m2))
	 (n (- rdim1 1))
	 (m (- (matrix-cols m1) 1))
	 (p (- rdim2 1))
	 (result (make-matrix rdim1 rdim2)))
    (loop
       for i from 0 to n
       do
	 (loop
	    for j from 0 to p
	    do
	      (setf (m-ref result i j)
		    (loop
		       for k from 0 to m
		       summing (* (m-ref m1 i k) (m-ref m2 k j)) 
		       into res
		       finally (return res)))))
    result))

(defmethod matrix->grid (m)
  (let* ((dim1 (matrix-rows m))
	 (dim2 (matrix-cols m))
	 (result (grid:make-foreign-array 'double-float :dimensions `(,dim1 ,dim2)
					  :initial-contents
					  (loop
					     for i below dim1
					     collect
					       (loop
						  for j below dim2
						  collect
						    (m-ref m i j))))))
    result))

(defun grid->matrix (g)
  (let* ((a (grid:cl-array g))
	 (dim1 (array-dimension a 0))
	 (dim2 (array-dimension a 1)))
    (print dim1)
    (print dim2)
    (make-matrix dim1 dim2 (array->list a))))

(defun covariance-matrix (point-set)
  "Compute the coveraiance of the set of points."
  (let* ((centroid (point-centroid point-set))
         (centered-points (mapcar #'(lambda (x) (v- (point->vector x) (point->vector centroid))) point-set))
         (num-points (length centered-points))
         (covariance
          (mult-matrix 
           (loop
              with result-tp = (make-array '(3 3) :element-type 'number :initial-element 0)
              for pt in centered-points
              do
                (let* ((tp (tensor-product pt pt)))
                  (setf result-tp (add-matrices result-tp tp)))
              finally (return result-tp))
           (/ num-points))))
    covariance))
