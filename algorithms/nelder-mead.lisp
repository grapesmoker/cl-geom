(in-package :geom)

;; An implementation of the Nelder-Mead simplex optimization algorithm

(defun nelder-mead-2d (f-to-minimize starting-point step-size &optional (epsilon 1e-3))
  "An implementation of the Nelder-Mead function minimization method in 2D."
  (declare (optimize (debug 3)))
  (the cs::point starting-point)
  (the function f-to-minimize)
  (the number step-size)
  (flet ((create-simplex (pt step-size)
           (make-polygon (list (make-point (point-x pt) (point-y pt) 0.0)
                               (make-point (+ (point-x pt) step-size) (point-y pt) 0.0)
                               (make-point (point-x pt) (+ (point-y pt) step-size) 0.0)))))
    (loop
       with current-simplex = (create-simplex starting-point step-size)
       with current-best-values = nil
       for i upfrom 1
       ;; do 
       ;;      (format t "before~%")
       ;;      (format t "~A~%" (every (mapcar #'(lambda (pair)
       ;;                                          (abs (- (first pair) (second pair))))
       ;;                                      (permutations-with-repetition current-best-values 2))
       ;;                              #'(lambda (x)
       ;;                                  (< x epsilon))))
       ;;      (format t "after~%")
       until (and current-best-values
                  (every  #'(lambda (x)
                              (< x epsilon))
                          (mapcar #'(lambda (pair)
                                      (abs (- (first pair) (second pair))))
                                  (permutations-with-repetition current-best-values 2))
                          ))
       do
         (let* ((simplex-values (mapcar f-to-minimize (polygon-points current-simplex)))
                (worst-index (position (apply #'max simplex-values) simplex-values))
                (best-index (position (apply #'min simplex-values) simplex-values))
                (middle-index (car (remove worst-index (remove best-index '(0 1 2)))))
                (centroid (point-centroid (list (elt (polygon-points current-simplex) best-index)
                                                (elt (polygon-points current-simplex) middle-index))))
                (best-point (elt (polygon-points current-simplex) best-index))
                (worst-point (elt (polygon-points current-simplex) worst-index))
                (middle-point (elt (polygon-points current-simplex) middle-index))
                (alpha 1)
                (beta 0.5)
                (gamma 2)
                (delta 0.5)
                (reflection-point (p+ centroid (p*s (p- centroid worst-point) alpha)))
                (r-value (funcall f-to-minimize reflection-point)))
           ;;(format t "current simplex: ~A~%" current-simplex)
           ;;(format t "best: ~A, worst: ~A, middle: ~A~%" best-point worst-point middle-point)
           ;; (format t "centroid: ~A~%" centroid)
           ;; (format t "reflection: ~A~%" reflection-point)
           ;;(format t "best value: ~F, middle value: ~F, worst value: ~F~%" 
           ;;      (elt simplex-values best-index)
           ;;      (elt simplex-values middle-index)
           ;;      (elt simplex-values worst-index))
           (cond ((and (< r-value (elt simplex-values middle-index))
                       (>= r-value (elt simplex-values best-index)))
                  ;;(format t "reflection~%")
                  (setf current-simplex (make-polygon (list best-point middle-point reflection-point)))
                  (setf current-best-values (list (elt simplex-values best-index)
                                                  (elt simplex-values middle-index)
                                                  r-value)))
                 ((< r-value (elt simplex-values best-index))
                  ;;(format t "expansion~%")
                  (let* ((expansion-point (p+ centroid (p*s (p- reflection-point centroid) gamma)))
                         (e-value (funcall f-to-minimize expansion-point)))
                    (if (< e-value r-value)
                        (progn
                          ;;(format t "accepting expansion~%")
                          (setf current-simplex (make-polygon (list best-point middle-point expansion-point)))
                          (setf current-best-values (list (elt simplex-values best-index)
                                                          (elt simplex-values middle-index)
                                                          e-value)))
                        (progn
                          ;;(format t "accepting reflection~%")
                          (setf current-simplex (make-polygon (list best-point middle-point reflection-point)))
                          (setf current-best-values (list (elt simplex-values best-index)
                                                          (elt simplex-values middle-index)
                                                          r-value))))))
                 ((>= r-value (elt simplex-values middle-index))
                  ;;(format t "contraction~%")
                  (if (< r-value (elt simplex-values best-index))
                      (let* ((contraction-point (p+ centroid (p*s (p- reflection-point centroid) beta)))
                             (c-value (funcall f-to-minimize contraction-point)))
                        (cond ((<= c-value r-value)
                               ;;(format t "accepting outside contraction~%")
                               (setf current-simplex (make-polygon (list best-point middle-point contraction-point)))
                               (setf current-best-values (list (elt simplex-values best-index)
                                                               (elt simplex-values middle-index)
                                                               c-value)))
                              (t
                               (setf current-simplex (make-polygon 
                                                      (list
                                                       best-point
                                                       (p+ best-point (p*s (p- worst-point best-point) delta))
                                                       (p+ best-point (p*s (p- middle-point best-point) delta)))))
                               (setf current-best-values 
                                     (list (elt simplex-values best-index)
                                           (funcall f-to-minimize (second (polygon-points current-simplex)))
                                           (funcall f-to-minimize (third (polygon-points current-simplex))))))))
                      (let* ((contraction-point (p+ centroid (p*s (p- worst-point centroid) beta)))
                             (c-value (funcall f-to-minimize contraction-point)))
                        (cond ((< c-value (elt simplex-values worst-index))
                               ;;(format t "accepting inside contraction~%")
                               (setf current-simplex (make-polygon (list best-point middle-point contraction-point))))
                              (t
                               (setf current-simplex (make-polygon 
                                                      (list
                                                       best-point
                                                       (p+ best-point (p*s (p- worst-point best-point) delta))
                                                       (p+ best-point (p*s (p- middle-point best-point) delta)))))
                               (setf current-best-values 
                                     (list (elt simplex-values best-index)
                                           (funcall f-to-minimize (second (polygon-points current-simplex)))
                                           (funcall f-to-minimize (third (polygon-points current-simplex))))))))))
                 (t (error "something has gone very wrong!"))))
       finally (return current-simplex))))
