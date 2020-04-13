;;;; nlopt.lisp

(cl:in-package #:nlopt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :c :nlopt.cffi))


;;; Library Loading 
(cffi:define-foreign-library nlopt
  (:windows "libnlopt.dll"))

(pushnew (asdf:system-source-directory :nlopt) cffi:*foreign-library-directories*)
(cffi:load-foreign-library 'nlopt)

;;; nlopt object 
(defclass nlopt ()
  ((ptr :initarg :ptr :reader ptr)
   (dimension :initarg :dimension :reader dimension)
   (objective :reader objective)
   (callbacks :accessor callbacks :initform nil)
   (identifiers :accessor identifiers :initform nil)))

;;; Mechanism for callbacks
(defparameter *nlopt-instance* nil
  "This variable is bound to the nlopt instance beign called")

(cffi:defcstruct callback-identifier
  (n :int))

(defmethod add-new-callback ((nlopt nlopt) (function function))
  "Adds a callback to nlopt object and returns an struct to identify that callback"
  (let ((identifier (cffi:foreign-alloc '(:struct callback-identifier)))
		(callbacks (callbacks nlopt)))
	(setf callbacks (cons function callbacks))
	(setf (cffi:foreign-slot-value identifier '(:struct callback-identifier) 'n)
		  (length callbacks))
	(push identifier (identifiers nlopt))
	identifier))

(cffi:defcallback objective-callback :double
	((n :unsigned-int) (x :pointer) (grad :pointer) (user_data :pointer))
  (declare (ignore n user_data))
  (funcall (objective *nlopt-instance*)
		   x
		   (unless (cffi:null-pointer-p grad)
			 grad)
		   *nlopt-instance*))


(cffi:defcallback constraint-callback :double
	((dimension :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  (declare (ignore dimension))
  (cffi:with-foreign-slots ((n) data (:struct callback-identifier))
	(funcall (nth n (callbacks *nlopt-instance*))
			 x
			 (unless (cffi:null-pointer-p grad)
			   grad)
			 *nlopt-instance*)))

;;; Bindings

(defun algorithms ()
  (cffi:foreign-enum-keyword-list 'c:nlopt_algorithm))

(defun create (algorithm dimension)
  (assert  (and (integerp dimension)
				(> dimension 0 ))
		   (dimension))
  (assert (member algorithm (algorithms)) (algorithm))
  (let ((ptr (c:create (cffi:foreign-enum-value 'c:nlopt_algorithm algorithm) dimension)))
	(unless (cffi:null-pointer-p ptr)
	  (make-instance 'nlopt
					 :dimension dimension
					 :ptr ptr))))

	
(defmethod set-lower-bounds ((nlopt nlopt) (bounds array))
  (assert (= (length bounds) (dimension nlopt)))
  (assert (eql (array-element-type bounds) 'double-float))
  (cffi:with-pointer-to-vector-data (ptr bounds)
	(c:set_lower_bounds (ptr nlopt) ptr))
  nlopt)

(defmethod set-min-objective ((nlopt nlopt) (function function))
  (c:set_min_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))

(defmethod set-max-objective ((nlopt nlopt) (function function))
  (c:set_max_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))



(defmethod add-equality-constraint ((nlopt nlopt) (function function) (tol double-float))
  (let ((callback-identifier (add-new-callback nlopt function)))
	(c:add_equality_constraint (ptr nlopt)
							   (cffi:get-callback 'constraint-callback)
							   callback-identifier
							   tol)
	callback-identifier))

(defmethod add-inequality-constraint ((nlopt nlopt) (function function) (tol double-float))
  (let ((callback-identifier (add-new-callback nlopt function)))
	(c:add_equality_constraint (ptr nlopt)
							   (cffi:get-callback 'constraint-callback)
							   callback-identifier
							   tol)
	callback-identifier))


(defmethod set-xtol-rel ((nlopt nlopt) (tol double-float))
  "Set x tolerace relative"
  (c:set_xtol_rel (ptr nlopt) tol))

(defmethod optimize-nlp ((nlopt nlopt) &optional (initial-x nil x-p))
  "Optimize the NonLinear Optimization problem nlopt with initial point initial-x"
  (if x-p
	  (progn (assert (= (length initial-x) (dimension nlopt)))
			 (assert (eql (array-element-type initial-x) 'double-float)))
	  (progn
		(setf initial-x (make-array (dimension nlopt)
									:initial-element 0d0
									:element-type 'double-float))))
  (cffi:with-foreign-object (f-value :double)
	(let ((*nlopt-instance* nlopt))
	  (declare (special *nlopt-instance*))
	  (cffi:with-pointer-to-vector-data (ptr initial-x)
		(c:optimize_nlp (ptr nlopt) ptr f-value)
		(values initial-x (cffi:mem-ref f-value :double))))))
