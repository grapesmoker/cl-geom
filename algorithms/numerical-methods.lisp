
(in-package :geom)

(defun sgn (x)
  (assert (numberp x))
  (cond ((= x 0)
	 '0)
	((> x 0)
	 '+)
	((< x 0)
	 '-)))

(defun sgn-numeric (x)
  (assert (numberp x))
  (cond ((= x 0)
	 0)
	((> x 0)
	 1)
	((< x 0)
	 -1)))


(defun d/dx (f-of-x x0 &optional (h 1e-8))
  "Take the numerical derivative of a function sampled at x0, with an optional h"
  ())

(defun d/dx-num (x-vals y-vals x0 &optional (h 1e-8))
  "Take the derivative of the vector representation of a function at x0, with optional h."
  (declare (ignore h))
  (assert (and (>= x0 (aref x-vals 0))
	       (<= x0 (aref x-vals (- (length x-vals) 1)))
	       (= (length x-vals) (length y-vals))))
  ;; find the interval into which x0 falls
  (multiple-value-bind (int-start int-end)
      (loop
	 with start = 0
	 with end = 1
	 for i from 0 to (- (length x-vals) 2)
	 if (and (>= x0 (aref x-vals start))
		 (<= x0 (aref x-vals end)))
	 do (return (values start end))
	 else
	 do
	   (incf start)
	   (incf end)
	 finally
	   (return (values nil nil)))
    ;;(format t "start: ~F, end: ~F~%" int-start int-end)
    ;;(format t "~F falls between ~F and ~F~%" x0 (aref x-vals int-start) (aref x-vals int-end))
    (when (and int-start int-end)
      ;; do a linear interp between the endpoints
      (let* ((y-start (aref y-vals int-start))
	     (y-end (aref y-vals int-end))
	     (x-start (aref x-vals int-start))
	     (x-end (aref x-vals int-end))
	     (m (/ (- y-end y-start) (- x-end x-start))))
	     ;;(b (- y-start (* m x-start)))
	     ;;(y0 (+ (* m x0) b)))
	m))))

(defun d/dx-lin-interp (x-vals y-vals)
  "Take the derivative of y-vals at every point of x-vals using simple linear interpolation."
  (assert (= (length x-vals) (length y-vals)))
  (let* ((num-entries (length x-vals))
	 (derivative (make-array num-entries :element-type 'number)))
    (loop
       for x-val across x-vals
       for y-val across y-vals
       for i upfrom 0
       for j upfrom 1 to (- num-entries 1)
       do
	 (let* ((next-x-val (aref x-vals j))
		(next-y-val (aref y-vals j))
		(m (/ (- next-y-val y-val) (- next-x-val x-val))))
	   (setf (aref derivative i) m)))
    derivative))

(defun d/dx-centered (y-vals &optional (h 1e-8))
  "Take the derivative of y-vals at every point using centered differentiation."
  ())

(defun local-extrema (vec extrema-type)
  "Find all the local extrema in a vector. Return a list of indices corresponding to those extrema.
extrema-type is a symbol, either :max or :min, indicating which type of extrema is sought."
  (assert (or (eq extrema-type :max)
	      (eq extrema-type :min)))
  (loop
     with previous-val = (aref vec 0)
     for val across vec
     for i from 1 to (- (length vec) 1)
     if
       (case extrema-type
	 (:min
	  (and (>= (aref vec i) val)
	       (<= val previous-val)))
	 (:max
	  (and (<= (aref vec i) val)
	       (>= val previous-val)))
	 (otherwise
	  nil))
     collect
       (- i 1) into minima
     do
       ;;(format t "previous-val: ~F, val: ~F, next-val: ~F~%" previous-val val (aref vec i))
       (setf previous-val val)
     finally (return minima)))
	
(defun minimize-function (f start end &optional (num-points 1000))
  "Minimize a one-dimensional function on the domain [start, end]. If the function is not already
a vector, compute its values across the whole domain. If you pass a vectored representation of the
function, any num-points argument will be ignored. If the function is not properly defined over
the desired domain, safety is not guaranteed."
  (let* ((y-vals 
	  (typecase f
	    (function
	     (make-linear-vector f start end num-points))
	    (array
	     f)))
	 (x-vals (make-linear-vector #'(lambda (x) x) start end (length y-vals))))
    (assert (= (length x-vals) (length y-vals)))
    (let* ((y-minima (local-extrema y-vals :min))
	   (y-min-index
	    (loop
	       with current-min = (first y-minima)
	       for min in y-minima
	       if 
		 (<= (aref y-vals min) (aref y-vals current-min))
	       do
		 (setf current-min min)
	       finally
		 (return current-min)))
	   (x-min-value (aref x-vals y-min-index))
	   (y-min-value (aref y-vals y-min-index)))
      (values y-min-index x-min-value y-min-value))))

(defun parabola (x)
  (+ 2 (expt (- x 4) 2)))

(defun make-linear-vector (f start end num-entries)
  "Given a one-variable function and a start and end, generated a vector with num-entries
whose values are the values of the function on those regularly spaced points."
  (let ((new-vector (make-array num-entries :element-type 'number))
	(step-size (/ (- end start) num-entries)))
    (loop
       with current-value = start
       for i from 0 to (- num-entries 1)
       do
	 (setf (aref new-vector i) (funcall f current-value))
	 (incf current-value step-size)
       finally (return new-vector))))
