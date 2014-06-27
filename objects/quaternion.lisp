(in-package :geom)

(defclass quaternion (geometric-object)
  ((w :accessor quat-w
      :initform nil
      :initarg :w)
   (x :accessor quat-x
      :initform nil
      :initarg :x)
   (y :accessor quat-y
      :initform nil
      :initarg :y)
   (z :accessor quat-z
      :initform nil
      :initarg :z)))
    
(defmethod components ((q quaternion))
  (list (quat-w q)
	(quat-x q)
	(quat-y q)
	(quat-z q)))

(defun make-quaternion (w x y z)
  (assert
   (every #'(lambda (component) (or (numberp component)
				    (eq nil component)))
	  (list w x y z)))
  (make-instance 'quaternion :w w :x x :y y :z z))

(defmethod is-geom-valid? ((q quaternion))
  (declare (ignore args 3d))
  (every #'(lambda (component) (numberp component))
	 (components q)))
	    
(defmethod q+ ((q1 quaternion) (q2 quaternion))
  (make-quaternion (+ (quat-w q1) (quat-w q2))
		   (+ (quat-x q1) (quat-x q2))
		   (+ (quat-y q1) (quat-y q2))
		   (+ (quat-z q1) (quat-z q2))))

(defmethod q+ ((q quaternion) (v vect))
  (make-quaternion (quat-w q)
		   (+ (quat-x q) (vect-x v))
		   (+ (quat-y q) (vect-y v))
		   (+ (quat-z q) (vect-z v))))

(defgeneric q* (q1 q2)
  (:documentation "Mutliply the quaternion q1 by either a number or a quaternion q2."))

(defmethod q* ((q quaternion) (s number))
  (make-quaternion (* s (quat-w q))
		   (* s (quat-x q))
		   (* s (quat-y q))
		   (* s (quat-z q))))

(defmethod q* ((q1 quaternion) (q2 quaternion))
  (let* ((w1 (quat-w q1)) (x1 (quat-x q1)) (y1 (quat-y q1)) (z1 (quat-z q1))
	 (w2 (quat-w q2)) (x2 (quat-x q2)) (y2 (quat-y q2)) (z2 (quat-z q2)))
    (make-quaternion (+ (* w1 w2) (- (* x1 x2)) (- (* y1 y2)) (- (* z1 z2)))
		     (+ (* w2 x1) (* x2 w1) (- (* y2 z1)) (* z2 y1))
		     (+ (* w2 y1) (* x2 z1) (* y2 w1) (- (* z2 x1)))
		     (+ (* w2 z1) (- (* x2 y1)) (* y2 x1) (* z2 w1)))))

(defmethod norm ((q quaternion))
  (+ (* (quat-w q) (quat-w q))
     (* (quat-x q) (quat-x q))
     (* (quat-y q) (quat-y q))
     (* (quat-z q) (quat-z q))))

(defmethod normalize ((q quaternion))
  (q* q (/ (geom-modulus q))))

(defmethod geom-modulus ((q quaternion))
  (sqrt (norm q)))

(defmethod inverse ((q quaternion))
  (when (is-geom-valid? q)
    (q* (conj q) (/ 1 (norm q)))))

(defmethod conj ((q quaternion))
  (when (is-geom-valid? q)
    (make-quaternion (quat-w q)
		     (- (quat-x q))
		     (- (quat-y q))
		     (- (quat-z q)))))


(defgeneric q-rotate (quaternion object)
  (:documentation "Apply the rotation defined by the given quaternion to the object. The result is a new object of the same type."))

(defmethod q-rotate ((q quaternion) (p point))
  (vector->point (q-rotate q (point->vector p))))

(defmethod q-rotate ((q quaternion) (v vect))
  (let ((qv (vect-component q))
	(qw (quat-w q)))
    (v+ 
     (v+ (v* v (- (* qw qw) (* (norm qv) (norm qv))))
	 (v* qv (* 2 (v-dot v qv))))
     (v* (v-cross qv v) (* 2 qw)))))

(defmethod q-rotate* ((q quaternion) (p point))
  "A special form of q-rotate which operates in place on the object, rather than returning a value."
  (set-coords p (q-rotate q p)))

(defmethod axis-angle->quaternion ((angle number) (axis vect))
  (let* ((half-angle (/ angle 2))
	 (scaled-axis (v* axis (sin half-angle))))
    (make-quaternion (cos half-angle)
		     (vect-x scaled-axis)
		     (vect-y scaled-axis)
		     (vect-z scaled-axis))))

(defmethod vect-component ((q quaternion))
  (make-vect (quat-x q) (quat-y q) (quat-z q)))

(defmethod quaternion->axis-angle ((q quaternion))
  (let* ((angle (* 2 (atan (norm (vect-component q)) (quat-w q))))
	 (axis (vect-component q)))
    (values (normalize axis) angle)))

(defmethod quaternion->matrix ((q quaternion))
  (let ((q0 (quat-w q))
	(q1 (quat-x q))
	(q2 (quat-y q))
	(q3 (quat-z q))
	(matrix (make-matrix 3 3)))
    (setf (m-ref matrix 0 0) (+ (expt q0 2) (expt q1 2) (- (expt q2 2)) (- (expt q3 2))))
    (setf (m-ref matrix 0 1) (* 2 (- (* q1 q2) (* q0 q3))))
    (setf (m-ref matrix 0 2) (* 2 (+ (* q1 q3) (* q0 q2))))

    (setf (m-ref matrix 1 0) (* 2 (+ (* q1 q2) (* q0 q3))))
    (setf (m-ref matrix 1 1) (+ (expt q0 2) (- (expt q1 2)) (expt q2 2) (- (expt q3 2))))
    (setf (m-ref matrix 1 2) (* 2 (- (* q2 q3) (* q0 q1))))

    (setf (m-ref matrix 2 0) (* 2 (- (* q1 q3) (* q0 q2))))
    (setf (m-ref matrix 2 1) (* 2 (+ (* q2 q3) (* q0 q1))))
    (setf (m-ref matrix 2 2) (+ (expt q0 2) (- (expt q1 2)) (- (expt q2 2)) (expt q3 2)))

    matrix))
    

(defmethod matrix->quaternion ((m array))
  (assert (equalp '(3 3) (array-dimensions m)))
  (labels ((sign (x) (if (>= x 0.0) 1.0 -1.0))
	   (zero-bound (x) (if (> x 0.0) x 0)))
    (let* ((r11 (aref m 0 0)) (r12 (aref m 0 1)) (r13 (aref m 0 2))
	   (r21 (aref m 1 0)) (r22 (aref m 1 1)) (r23 (aref m 1 2))
	   (r31 (aref m 2 0)) (r32 (aref m 2 1)) (r33 (aref m 2 2))
	   (q0 (sqrt (zero-bound (/ (+ r11 r22 r33 1.0) 4.0))))
	   (q1 (sqrt (zero-bound (/ (+ r11 (- r22) (- r33) 1.0) 4.0))))
	   (q2 (sqrt (zero-bound (/ (+ (- r11) r22 (- r33) 1.0) 4.0))))
	   (q3 (sqrt (zero-bound (/ (+ (- r11) (- r22) r33 1.0) 4.0)))))
      (cond ((and (>= q0 q1)
		  (>= q0 q2)
		  (>= q0 q3))
	     (progn
	       (setf q1 (* q1 (sign (- r32 r23))))
	       (setf q2 (* q2 (sign (- r12 r31))))
	       (setf q3 (* q3 (sign (- r21 r12))))))
	    ((and (>= q1 q0)
		  (>= q1 q2)
		  (>= q1 q3))
	     (progn
	       (setf q0 (* q0 (sign (- r32 r23))))
	       (setf q2 (* q2 (sign (+ r21 r12))))
	       (setf q3 (* q3 (sign (+ r13 r31))))))
	    ((and (>= q2 q0)
		  (>= q2 q1)
		  (>= q2 q3))
	     (progn
	       (setf q0 (* q0 (sign (- r13 r31))))
	       (setf q1 (* q1 (sign (+ r21 r12))))
	       (setf q3 (* q3 (sign (+ r31 r23))))))
	    ((and (>= q3 q0)
		  (>= q3 q1)
		  (>= q3 q2))
	     (progn
	       (setf q0 (* q0 (sign (- r21 r12))))
	       (setf q1 (* q1 (sign (+ r31 r13))))
	       (setf q2 (* q2 (sign (+ r32 r23)))))))
      (normalize (make-quaternion q0 q1 q2 q3)))))
	    
	    

(defmethod print-object ((q quaternion) stream)
  (format stream "<#QUATERNION (~{~3,6F~^, ~})>" (components q)))


  ;; (let ((w (quat-w q)) (x1 (quat-x q)) (y1 (quat-y q)) (z1 (quat-z q))
  ;; 	(x2 (point-x p)) (y2 (point-y p)) (z2 (point-z p)))
  ;;   (make-point (+ (* x2 (- 1 (* 2 y1 y1) (* 2 z1 z1)))
  ;; 		   (* y2 (* 2 (+ (* x1 y1) (* w z1))))
  ;; 		   (* z2 (* 2 (- (* x1 z1) (* w y1)))))

  ;; 		(+ (* x2 (* 2 (- (* x1 y1) (* w z1))))
  ;; 		   (* y2 (- 1 (* 2 x1 x1) (* 2 z1 z1)))
  ;; 		   (* z2 (* 2 (+ (* y1 z1) (* w x1)))))

  ;; 		(+ (* x2 (* 2 (+ (* x1 z1) (* w y1))))
  ;; 		   (* y2 (* 2 (- (* y1 z1) (* w y1))))
  ;; 		   (* z2 (- 1 (* 2 x1 x1) (* 2 y1 y1)))))))
