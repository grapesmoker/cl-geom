;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;  (ql:quickload :gsll))

(defsystem "geometry"
  :name "geometry"
  :description "General pupose geometric objects and algorithms."
  :version "0.1"
  :author "Jerry Vinokurov <jerryv@cmu.edu>"
  :license "GPL v3"
  :defsystem-depends-on (#:gsll #:cl-graph) ;; #:lisp-unit2)
  :depends-on (#:gsll #:antik) ;; #:lisp-unit2)
  :components 
  ((:module objects
	    :components
	    ((:file "geometric-object")
	     (:file "conditions")
	     (:file "point" :depends-on ("geometric-object"))
	     (:file "vector" :depends-on ("point"))
	     (:file "matrix" :depends-on ("vector"))
	     (:file "quaternion" :depends-on ("vector" "matrix"))
	     (:file "line-segment" :depends-on ("vector" "matrix"))
	     (:file "polygon" :depends-on ("line-segment" "quaternion"))
	     (:file "geometry" :depends-on ("polygon"))
	     (:file "test" :depends-on ("geometry"))))
   (:module algorithms
	    :depends-on (objects)
	    :components
	    ((:file "numerical-methods")
	     (:file "polyline-matching")
	     (:file "icp" :depends-on ("numerical-methods"))
	     (:file "constraint")
	     (:file "solver" :depends-on ("constraint"))))))


(defpackage :geometry
  (:nicknames :geom)
  (:use :cl :asdf :gsll) ;; :lisp-unit2)
  (:shadow :norm)
  (:documentation "A general purpose geometry library. Includes support for points, 
line segments, polygons, 3-vectors, 4-vectors, quaternions, and hopefully lots more")
  (:export #:point
	   #:point-x
	   #:point-y
	   #:point-z
	   #:line-segment
	   #:distance
	   #:make-line
	   #:make-point
	   #:start-point
	   #:end-point
	   #:make-constraint))
