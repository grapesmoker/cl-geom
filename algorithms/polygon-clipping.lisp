(in-package :geom)

;; An implementation of the Greiner-Hormann algorithm for polygon
;; clipping

;; A special vertex class for the GH algorithm. Don't use outside this file.

(defclass gh-vertex ()
  ((point :accessor gh-vertex-point
          :initform nil
          :initarg :point)
   (next  :accessor gh-next-vertex
          :initform nil
          :initarg :next)
   (prev  :accessor gh-prev-vertex
          :initform nil
          :initarg :prev)
   (intersect :accessor gh-vertex-int
              :initform nil
              :initarg :intersect)
   (status :accessor gh-vertex-status
           :initform nil
           :initarg :status)
   (neighbor :accessor gh-vertex-neighbor
             :initform nil
             :initarg :status)
   (alpha :accessor gh-vertex-alpha
          :initform 0
          :initarg :alpha)
   (processed :accessor gh-vertex-processed
              :initform nil
              :initarg :processed)
   (next-poly :accessor gh-next-poly
              :initform nil
              :initarg :next-poly)))

(defmethod polygon->gh-vertices ((p polygon))
  (loop 
     with first-p = (first (polygon-points p))
     with first-vertex = (make-instance 'gh-vertex :point first-p)
     with current-vertex = first-vertex
     for i upfrom 0
     for next-p in (rest (polygon-points p))
     do
       (let* ((new-vertex (make-instance 'gh-vertex :point next-p :prev current-vertex)))
         (setf (gh-next-vertex current-vertex) new-vertex)
         (setf current-vertex new-vertex))
     finally
       (setf (gh-next-vertex current-vertex) first-vertex)
       (setf (gh-prev-vertex first-vertex) current-vertex)
       (return first-vertex)))

(defmethod insert-vertex-between ((v1 gh-vertex) (v2 gh-vertex) (new-vertex gh-vertex))
  "Insert a vertex between the two specified vertices in the doubly linked list.
If a vertex already exists between these two, then insert by alpha value."
  (cond ((and (eql (gh-next-vertex v1) v2)
              (eql (gh-prev-vertex v2) v1))
         ;; if the two vertices are adjacent and there's not anything between them
         (setf (gh-next-vertex v1) new-vertex)
         (setf (gh-prev-vertex new-vertex) v1)
         (setf (gh-next-vertex new-vertex) v2)
         (setf (gh-prev-vertex v2) new-vertex))
        ;; if there's already something there...
        (t
         (loop
            with current-vertex = (gh-next-vertex v1)
            while (and (< (gh-vertex-alpha current-vertex) (gh-vertex-alpha new-vertex))
                       (not (eql current-vertex v2)))
            do
              (setf current-vertex (gh-next-vertex current-vertex))
            finally
              (setf (gh-next-vertex new-vertex) current-vertex)
              (setf (gh-prev-vertex new-vertex) (gh-prev-vertex current-vertex))
              (setf (gh-next-vertex (gh-prev-vertex current-vertex)) new-vertex)
              (setf (gh-prev-vertex current-vertex) new-vertex)))))

(defmethod greiner-hormann ((subject polygon) (clip polygon))
    ;; first we have to turn the polygons into a doubly-linked list of vertices
    (let* ((s-vertex-head (polygon->gh-vertices subject))
           (c-vertex-head (polygon->gh-vertices clip))
           ;; (dummy 
           ;;  (progn
           ;;    (format t "subject:~%~A~%" s-vertex-head)
           ;;    (format t "clip:~%~A~%" c-vertex-head)))
           ;; phase 1 - intersection
           (new-points
            (loop
               with first-sv = s-vertex-head
               with current-sv = s-vertex-head
               with int-points = '()
               with int-vertices = '()
               ;;with i = 1
                 
               do
               ;;(format t "current-cv: ~A~%" (gh-vertex-point current-sv))
                 (loop
                    with first-cv = c-vertex-head
                    with current-cv = c-vertex-head
                      
                    do
                    ;;(format t "current-sv: ~A~%" (gh-vertex-point current-cv))
                      (let* ((sp1 (gh-vertex-point (gh-prev-vertex current-sv)))
                             (ep1 (gh-vertex-point current-sv))
                             (sp2 (gh-vertex-point (gh-prev-vertex current-cv)))
                             (ep2 (gh-vertex-point current-cv))
                             (ls1 (make-line sp1 ep1))
                             (ls2 (make-line sp2 ep2)))
                        ;;(format t "line 1: ~A~%" ls1)
                        ;;(format t "line 2: ~A~%" ls2)
                        (multiple-value-bind (int p-int)
                            (intersect-parametric? ls1 ls2)
                          (when (and int (not (find p-int int-points :test #'geom=)))
                            (let* ((alpha1 (/ (distance p-int sp1) (distance sp1 ep1)))
                                   (alpha2 (/ (distance p-int sp2) (distance sp2 ep2)))
                                   (new-vertex1 (make-instance 'gh-vertex :point p-int :alpha alpha1))
                                   (new-vertex2 (make-instance 'gh-vertex :point p-int :alpha alpha2)))
                              ;;(format t "intersection at: ~A~%" p-int)
                              ;;(incf i)
                              ;;(insert-vertex-between (gh-prev-vertex current-sv) current-sv new-vertex1)
                              ;;(insert-vertex-between (gh-prev-vertex current-cv) current-cv new-vertex2)
                              (push p-int int-points)
                              (push (list (gh-prev-vertex current-sv) current-sv new-vertex1) int-vertices)
                              (push (list (gh-prev-vertex current-cv) current-cv new-vertex2) int-vertices)
                              ;;(format t "pushed~%")
                              (setf (gh-vertex-neighbor new-vertex1) new-vertex2)
                              (setf (gh-vertex-neighbor new-vertex2) new-vertex1)
                              (setf (gh-vertex-int new-vertex1) t)
                              (setf (gh-vertex-int new-vertex2) t)
                              ;;(format t "here~%")
                              ))))
                      (setf current-cv (gh-next-vertex current-cv))
                    while (not (eq current-cv first-cv)))
                 (setf current-sv (gh-next-vertex current-sv))
               while (not (eq current-sv first-sv))
               finally (return int-vertices))))
      ;;(declare (ignore dummy))
      ;;(mapcar #'(lambda (x) (format t "~A~%" (gh-vertex-point (third x)))) new-points)
      (when new-points 
        (loop
           for insertion in new-points
           do
           ;;(format t "insertion: ~A~%" (gh-vertex-point (third insertion)))
             (insert-vertex-between (first insertion) (second insertion) (third insertion)))
        ;;(print s-vertex-head)
        ;;(print c-vertex-head)
        
        ;; phase 2 - mark entry/exit status
        (loop
           with first-v = s-vertex-head
           with current-v = (gh-next-vertex s-vertex-head)
           with status = nil
           initially
             (if (point-in-polygon? clip (gh-vertex-point first-v))
                 (progn
                   (setf status :exit)
                   (setf (gh-vertex-status first-v) status))
                 (progn
                   (setf status :entry)
                   (setf (gh-vertex-status first-v) :entry)))
           do
             (if (gh-vertex-int current-v)
                 (progn
                   (setf (gh-vertex-status current-v) status)
                   (case status
                     (:exit (setf status :entry))
                     (:entry (setf status :exit)))))
             (setf current-v (gh-next-vertex current-v))
           until (eq current-v first-v))
        
        (loop
           with first-v = c-vertex-head
           with current-v = (gh-next-vertex c-vertex-head)
           with status = nil
           initially
             (if (point-in-polygon? clip (gh-vertex-point first-v))
                 (progn
                   (setf status :exit)
                   (setf (gh-vertex-status first-v) status))
                 (progn
                   (setf status :entry)
                   (setf (gh-vertex-status first-v) :entry)))
           do
           ;; (format t "~A~%" (gh-vertex-point current-v))
             (if (gh-vertex-int current-v)
                 (progn
                   (setf (gh-vertex-status current-v) status)
                   (case status
                     (:exit (setf status :entry))
                     (:entry (setf status :exit)))))
             (setf current-v (gh-next-vertex current-v))
           until (eq current-v first-v))
        ;; (format t "building polygons~%")
        
        ;; phase 3 - construct polygons
        
        (flet ((count-unprocessed-intersecting-vertices (head)
                 (loop
                    with ct = 0
                    with current-v = head
                    do 
                      (when (and (not (gh-vertex-processed current-v))
                                 (gh-vertex-int current-v))
                        ;;(format t "unprocessed vertex: ~A~%" (gh-vertex-point current-v))
                        (incf ct))
                      (setf current-v (gh-next-vertex current-v))
                    until (eq current-v head)
                    finally (return ct))))
          
          (loop
             with new-polygons = '()
             with current-sv = s-vertex-head
             ;;initially
             do
               (loop
                  until (and (gh-vertex-int current-sv)
                             (not (gh-vertex-processed current-sv)))
                  do
                  ;;(format t "advancing to point: ~A~%" (gh-vertex-point current-sv))
                    (setf current-sv (gh-next-vertex current-sv)))
             ;;(format t "~A~%" (gh-vertex-point current-sv))
               (loop
                  with new-poly = '()
                  for i upfrom 0
                  initially
                    (push (gh-vertex-point current-sv) new-poly)
                    (setf (gh-vertex-processed current-sv) t)
                  if (eq (gh-vertex-status current-sv) :entry)
                  do
                    (loop
                       do
                         (setf current-sv (gh-next-vertex current-sv))
                         (setf (gh-vertex-processed current-sv) t)
                         (push (gh-vertex-point current-sv) new-poly)
                       ;; (format t "points: ~A~%" (gh-vertex-point current-sv))
                       ;; (format t "status: entry~%")
                       ;; (format t "int: ~A~%" (gh-vertex-int current-sv))
                       ;; (format t "processed: ~A~%" (gh-vertex-processed current-sv))
                       until (gh-vertex-int current-sv))
                  else
                  do
                    (loop
                       do
                         (setf current-sv (gh-prev-vertex current-sv))
                         (setf (gh-vertex-processed current-sv) t)
                         (push (gh-vertex-point current-sv) new-poly)
                       ;; (format t "points: ~A~%" (gh-vertex-point current-sv))
                       ;; (format t "status: exit~%")
                       ;; (format t "int: ~A~%" (gh-vertex-int current-sv))
                       ;; (format t "processed: ~A~%" (gh-vertex-processed current-sv))
                       until (gh-vertex-int current-sv))
                  do
                    (setf current-sv (gh-vertex-neighbor current-sv))
                    (setf (gh-vertex-processed current-sv) t)
                  ;;(format t "polygon: ~A~%" new-poly)
                  ;; (format t "start: ~A~%" (first new-poly))
                  ;; (format t "end: ~A~%" (last new-poly))
                  ;; (format t "stop condition: ~A~%" 
                  ;; (and (> (length new-poly) 1)
                  ;;      (geom= (first new-poly) (car (last new-poly)))))
                  until (and (> (length new-poly) 1)
                             (geom= (first new-poly) (car (last new-poly))))
                  finally
                    (push (make-polygon new-poly) new-polygons))
             ;;do
             ;;  (format t "remaining vertices: ~D~%" (count-unprocessed-intersecting-vertices s-vertex-head))
             while (> (count-unprocessed-intersecting-vertices s-vertex-head) 0)
             finally (return new-polygons))
          ))))


(defmethod polygon-overlap ((p1 polygon) (p2 polygon) &optional (algorithm #'greiner-hormann))
  (funcall algorithm p1 p2))
