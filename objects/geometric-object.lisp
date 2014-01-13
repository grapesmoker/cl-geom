(in-package :geom)

;; A generic class that all geometric objects are descended from. The class defines some
;; generic methods that can be used on it. Any new generic definitions should go here.

(defclass geometric-object ()
  ((is-valid :accessor geom-is-valid?
	     :initform nil)))
 
(defgeneric pprint-geom (obj)
  (:documentation "Prints some sensible representation of the object."))

(defgeneric add-to-graph (obj graph)
  (:documentation "Adds the object to the constraint graph."))

(defgeneric node->geometry (node)
  (:documentation "Converts a node container into the geometry inside the container."))

(defmethod node->geometry ((node cl-graph:graph-container-vertex))
  (metabang.utilities:element node))

(defmethod node->geometry ((node geometric-object))
  node)

(defmethod edge->constraint ((edge cl-graph:graph-container-edge))
  (metabang.utilities:element edge))

(defgeneric geometry->stl (obj &optional terminator)
  (:documentation "Convert a geometric object into an STL vector for communicating with RFrame."))

(defgeneric geometry->json (obj)
  (:documentation "Converts a geometric object into a JSON representation."))

(defmethod geometry->json ((obj-list list))
  (loop
     for obj in obj-list
     collect
       (geometry->json obj)))

(defmethod geometry->stl ((obj-list list) &optional (terminator t))
  (loop
     for obj in obj-list
     collect
       (geometry->stl obj terminator)))

(defgeneric geometry->points (obj)
  (:documentation "Return all the points constituting the geometric object."))

(defgeneric set-height (obj value)
  (:documentation "A quite stupid helper to set the heigh (i.e. z coord) of any object"))

(defgeneric intersect? (obj1 obj2)
  (:documentation "Figure out whether the two objects intersect or not."))

(defgeneric copy-geometry (obj)
  (:documentation "Shallow-copy the geometry of the object."))

(defgeneric centroid (obj)
  (:documentation "Centroid of the object"))

(defgeneric rotate-object (obj angle ref-point)
  (:documentation "Rotate an object around a reference point by a given angle."))

(defgeneric rotate-objects (objects angle ref-point)
  (:documentation "Rotate all the objects in a list around a reference point by a given angle."))

(defgeneric reflect-object (object ref-line)
  (:documentation "Reflect an object around a reference line."))

(defgeneric reflect-objects (objects ref-line)
  (:documentation "Reflect all the objects in a list around a reference line."))

;; All algorithms typically depend on `valid' geometry to function. In this context,
;; validity means that all the constituent points of an object have a defined geometry.
;; There are times when you want to leave the geometry undefined; for example, when
;; you are running the constraint solver which might not know the location of some
;; of the endpoints. Algorithms which do this should specify that explicitly. For
;; everything else, you should use the `with-valid-geometry' macro to wrap all operations
;; that depend on geometric validity. This abstracts the error-checking to a higher level
;; and ensures that you will never encounter an unexpected NIL somewhere. 

(defmacro with-valid-geometry ((objects) &rest body)
  `(progn
     (unless (is-geom-valid? ,objects)
       (error 'invalid-geometry :geometric-object ,objects :error-text "Invalid geometry!"))
     ,@body))

;; Useful macro for making sure you're working with a list of numbers.

(defmacro with-numbers ((objects) &rest body)
  `(progn
     (unless (every #'numberp ,objects)
       (error 'invalid-numbers :numbers ,objects :error-text "Non-numbers passed where numbers were expected!" :geometric-object nil))
     ,@body))

;; Another useful macro for allowing either numbers or nils.

(defmacro with-numbers-or-nils ((objects) &rest body)
  `(progn
     (unless (every #'(lambda (x) (or (numberp x) (eq nil x))) ,objects)
       (error 'invalid-numbers :numbers ,objects :error-text "Non-numbers and non-NIL where numbers or NIL were expected!"))
     ,@body))
