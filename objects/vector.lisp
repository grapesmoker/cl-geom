(in-package :geom)

;; vector.lisp is an implementation of the vector class. Actually, vect itself is only the parent
;; of two other classes: vec3 and vec4. These correspond to (duh) the 3D and 4D vectors. Why?
;; Since we're dealing with geometry we will typically care about vectors in three dimensions.
;; Since we might want to do computer graphics with this, we might also care about 4D vectors.
;; We also have quaternions, but those aren't vectors, so it would be wrong to use them as such.

(defclass vect (geometric-object)
  ((contents :accessor vector-contents
	     :initarg :contents)))

(defclass vec4 (vect)
  ())

(defclass vec3 (vec4)
  ())

(defmethod vec3-x ((v vec3))
  (first (vector-contents v)))

(defmethod vec3-y ((v vec3))
  (second (vector-contents v)))

(defmethod vec3-z ((v vec3))
  (third (vector-contents v)))

(defmethod v-ref ((v vect) (i integer))
  (nth i (vector-contents v)))

(defmethod vec-size ((v vect))
  (length (vector-contents v)))

(defgeneric v+ (v obj)
  (:documentation "Add either two vectors or a vector and a scalar."))

(defgeneric translate (v p)
  (:documentation "Translate a point by the given vector."))

(defgeneric translate* (v p)
  (:documentation "Translate a point by the given vector. Operates in-place."))

(defun make-vec3 (&optional x y z)
  (with-numbers-or-nils ((list x y z))
    (make-instance 'vec3 :contents (list x y z))))

(defun make-vec4 (&optional x y z w)
  (with-numbers-or-nils ((list x y z w))
    (make-instance 'vec3 :contents (list x y z w))))

(defun make-vect (values)
  ;; create a vector containing as many values as you like
  (with-numbers-or-nils (values)
    (make-instance 'vect :contents values)))

(defmethod is-geom-valid? ((v vect))
  (every #'numberp (vector-contents v)))

(defmethod norm ((v vect))
  (with-valid-geometry (v)
    (sqrt (apply #'+ (vector-contents v)))))

(defmethod normalize ((v vect))
  (with-valid-geometry (v)
    (v* v (/ (norm v)))))

(defmethod point->vector ((p point))
  "Given a point, return a vector from the origin to that point. Points are 3D."
  (make-vec3 (point-x p) (point-y p) (point-z p)))

(defmethod vector->point ((v vec3))
  "Given a vector, return the point to which that vector points. Points are 3D."
  (make-point (vec3-x v) (vec3-y v) (vec3-z v)))

(defmethod vector->point ((v vect))
  (make-point (v-ref v 0) (v-ref v 1) (v-ref v 2)))

;; it's often a good idea to make sure all your vectors are the same size

(defmacro with-equal-size-vectors ((vectors) &rest body)
  (let ((g (gensym)))
    `(progn
       ;;(print ,vectors)
       (let ((,g (remove-duplicates (mapcar #'vec-size ,vectors) :test #'=)))
	 (when (> (length ,g) 1)
	   (error 'unequal-vectors :geometry-object ,vectors
		  :error-text "Unequal vectors used where all vectors have to be equal."))
	 ,@body))))

(defmethod v= ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (loop
	 for i below (vec-size v1)
	 if (not (= (v-ref v1 i) (v-ref v2 i)))
	 do (return nil)
	 finally (return t)))))

(defmethod v+ ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (let ((new-values
	     (loop
		for i below (vec-size v1)
		collect (+ (v-ref v1 i) (v-ref v2 i)))))
	(make-vect new-values)))))

(defmethod v- ((v1 vect) (v2 vect))
  (v+ v1 (v* v2 -1)))

(defun vec-sum (&rest vectors)
  (loop
     with sum = nil
     for vec in vectors
     do
       (if sum
	   (setf sum (v+ sum vec))
	   (setf sum vec))
     finally
       (return sum)))

(defmethod v+ ((v vect) (s number))
  "Strictly speaking the addition of a vector and a scalar is not a proper operation.
However, sometimes it's very useful to be able to add a constant term to all components
of a vector."
  (with-valid-geometry (v)
    (make-vect (mapcar #'(lambda (x) (+ s x)) (vector-contents v)))))

(defmethod v* ((v vect) (s number))
  (with-valid-geometry (v)
    (make-vect (mapcar #'(lambda (x) (* s x)) (vector-contents v)))))

(defmethod v-dot ((v1 vect) (v2 vect))
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (apply #'+
       (mapcar #'* (vector-contents v1) (vector-contents v2))))))

(defmethod v-cross ((v1 vec3) (v2 vec3))
  "The cross-product is only defined for vectors of dimension 3. Well, apparently it's
possible to define it in any dimensions using the Hodge star, but we won't worry
about that for now."
  (with-valid-geometry ((list v1 v2))
    (with-equal-size-vectors ((list v1 v2))
      (make-vec3 (- (* (vect-y v1) (vect-z v2))
		    (* (vect-y v2) (vect-z v1)))
		 (- (* (vect-z v1) (vect-x v2))
		    (* (vect-z v2) (vect-z v1)))
		 (- (* (vect-x v1) (vect-y v2))
		    (* (vect-x v2) (vect-y v1)))))))

(defmethod translate ((v vect) (p point))
  (with-valid-geometry ((list v p))
    (vector->point (v+ (point->vector p) v))))

(defmethod translate* ((v vect) (p point))
  (with-valid-geometry ((list v p))
    (set-coords p (translate v p))))
  
(defmethod print-object ((v vect) stream)
  (format stream "<#VECTOR (~{~3,6F~^, ~})>" (vector-contents v)))
		
