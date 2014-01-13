(in-package :geom)

(defun make-random-point ()
  (make-point (random 10.0)
	      (random 10.0)
	      (random 10.0)))

(defun make-random-points (num-points)
  (loop
     for i from 1 to num-points
     collect
       (make-random-point) into points
     finally (return points)))

(defun time-random-rotations (num-random-points num-tries)
  (let ((random-points (make-random-points num-random-points))
	(ref-point (make-random-point)))
    (time
     (dotimes (i num-tries)
       (rotate-objects random-points (deg->rad (random 360.0)) ref-point)))))

(lisp-unit:define-test test-make-vec3
  (let ((new-vec (make-vec3 1 2 3)))
    (loop
       for i below 3
       do
	 (lisp-unit:assert-equal (+ i 1) (v-ref new-vec i)))))
  
(lisp-unit:define-test test-vector-addition
  (let* ((v1 (make-vec3 1 2 3))
	 (v2 (make-vec3 4 5 6))
	 (result (v+ v1 v2)))
    (lisp-unit:assert-true (and (equalp (vector-contents result) '(5 7 9))))))
