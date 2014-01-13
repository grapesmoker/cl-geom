(in-package :geom)

(define-condition geometry-error (error)
  ((error-text :initarg :error-text
	       :accessor geometry-error-text)
   (error-log :initarg :error-log
	      :accessor geometry-error-log)
   (geometry-object :initarg :geometry-object
		    :accessor geometry-error-object
		    :initform nil))

  (:report (lambda (condition stream)
	     (format stream "Geometric error: ~A~%"
		     (geometry-error-text condition))))
  (:documentation "The parent class for all geometry errors."))

(define-condition geometry-warning (warning)
  ((warn-text :initarg :warn-text
	      :accessor geometry-warn-text))
  (:report (lambda (condition stream)
	     (format stream "Geometric warning: ~A~%"
		     (geometry-warn-text condition))))
  (:documentation "The parent class for all geometry warnings."))

(define-condition invalid-geometry (geometry-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Geometric error!~%Object: ~A~%Error: ~A~%"
		     (geometry-error-object condition)
		     (geometry-error-text condition))))
  (:documentation "Indicates invalid geometry being used."))

(define-condition invalid-numbers (geometry-error)
  ((number :initarg :numbers
	   :accessor geometry-error-numbers))
  (:report (lambda (condition stream)
	     (format stream "Geometric error!~%Object: ~A~%Error: ~A~%"
		     (geometry-error-object condition)
		     (geometry-error-text condition))))
  (:documentation "Indicates invalid numerical values being used in the indicated object."))

(define-condition unequal-vectors (geometry-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Geometric error!~%Object: ~A~%Error: ~A~%"
		     (geometry-error-object condition)
		     (geometry-error-text condition))))
  (:documentation "Indicates unequal vectors being used in an operation that requires same-length vectors."))
	    
(define-condition invalid-matrices (geometry-error)
  ()
  (:report (lambda (condition stream)
	     (format stream "Geometric error!~%Object: ~A~%Error: ~A~%"
		     (geometry-error-object condition)
		     (geometry-error-text condition))))
  (:documentation "Indicates invalid matrix that contains NILs somewhere it shouldn't."))
