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
			   if (not (numberp (aref m i j)))
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
    (let* ((dim1 (array-dimension m1 0))
	   (dim2 (array-dimension m1 1))
	   (result (make-array `(,dim1 ,dim2) :element-type 'number)))
      (loop
	 for i below dim1
	 do
	   (loop
	      for j below dim2
	      do
		(setf (aref result i j) (+ (aref m1 i j) (aref m2 i j)))))
      result)))


(defmethod tensor-product ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (let* ((matrix (make-array '(3 3) :element-type 'number))
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
		  (setf (aref matrix i j) (* v1-elem v2-elem))))
	matrix))))

(defun mult-matrix (m s)
  (let* ((dim1 (array-dimension m 0))
	 (dim2 (array-dimension m 1))
	 (result (make-array `(,dim1 ,dim2) :element-type 'number)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (aref result i j) (* (aref m i j) s))))
    result))

(defun transpose-matrix (m)
    (let* ((dim1 (array-dimension m 0))
	   (dim2 (array-dimension m 1))
	   (result (make-array `(,dim1 ,dim2) :element-type 'number)))
    (loop
       for i from 0 to (- dim1 1)
       do
	 (loop
	    for j from 0 to (- dim2 1)
	    do
	      (setf (aref result i j) (aref m j i))))
    result))

(defun matrix-trace (m)
  (let* ((dim1 (array-dimension m 0))
	 (dim2 (array-dimension m 1)))
    (assert (= dim1 dim2))
    (loop
       for i from 0 to (- dim1 1)
       summing (aref m i i) into result
       finally (return result))))

(defun matrix*vector (m v)
  ;; vectors are always size 3
  (assert (= (array-dimension m 1) 3))
  (let ((x (vect-x v)) (y (vect-y v)) (z (vect-z v)))
    (make-vect (+ (* (aref m 0 0) x)
		  (* (aref m 0 1) y)
		  (* (aref m 0 2) z))
	       (+ (* (aref m 1 0) x)
		  (* (aref m 1 1) y)
		  (* (aref m 1 2) z))
	       (+ (* (aref m 2 0) x)
		  (* (aref m 2 1) y)
		  (* (aref m 2 2) z)))))

(defun matrix*matrix (m1 m2)
  (assert (= (array-dimension m1 1)
	     (array-dimension m2 0)))
  (let* ((rdim1 (array-dimension m1 0))
	 (rdim2 (array-dimension m2 1))
	 (n (- rdim1 1))
	 (m (- (array-dimension m1 1) 1))
	 (p (- rdim2 1))
	 (result (make-array `(,rdim1 ,rdim2) :element-type 'number)))
    (loop
       for i from 0 to n
       do
	 (loop
	    for j from 0 to p
	    do
	      (setf (aref result i j)
		    (loop
		       for k from 0 to m
		       summing (* (aref m1 i k) (aref m2 k j)) 
		       into res
		       finally (return res)))))
    result))
