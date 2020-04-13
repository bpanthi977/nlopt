;;;; nlopt.lisp

(cl:in-package #:nlopt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :c :nlopt.cffi))

(cffi:define-foreign-library nlopt
  (:windows "libnlopt.dll"))

(pushnew "E:/tmp/nlopt/bin/" cffi:*foreign-library-directories*)

(cffi:load-foreign-library 'nlopt)

(defclass nlopt ()
  ((ptr :initarg :ptr :reader ptr)
   (dimension :initarg :dimension :reader dimension)
   (objective :reader objective)
   (callbacks :accessor callbacks)
   (identifiers :accessor identifiers)))

(defun algorithms ()
  (cffi:foreign-enum-keyword-list 'c:nlopt_algorithm))

(defun create (algorithm dimension)
  (print algorithm)
  (assert  (and (integerp dimension)
				(> dimension 0 ))
		   (dimension))
  (assert (member algorithm (algorithms)) (algorithm))
  (let ((ptr (c:create (cffi:foreign-enum-value 'c:nlopt_algorithm algorithm) dimension)))
	(unless (cffi:null-pointer-p ptr)
	  (make-instance 'nlopt
					 :dimension dimension
					 :ptr ptr))))

(deftype darray ()
  "Array of double floats"
  '(array double-float *))

(defgeneric darray (first &rest rest))

(defmethod darray ((first double-float) &rest rest)
  "Create an array of double-float"
  (make-array (1+ (length rest)) :element-type 'double-float :initial-contents (cons first rest)))

(declaim (inline dref))
(defun dref (ptr index)
  "Index a foreign array of double float"
  (cffi:mem-aref ptr :double index))

(defun (setf dref) (val ptr index)
  (setf (cffi:mem-aref ptr :double index) val))

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

	
(defmethod (setf lower-bounds) ((bounds array) (nlopt nlopt))
  (assert (= (length bounds) (dimension nlopt)))
  (assert (eql (array-element-type bounds) 'double-float))
  (cffi:with-pointer-to-vector-data (ptr bounds)
	(c:set_lower_bounds (ptr nlopt) ptr))
  nlopt)

(defmethod (setf min-objective) ((function function) (nlopt nlopt))
  (c:set_min_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))

(defmethod (setf max-objective) ((function function) (nlopt nlopt))
  (c:set_max_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))



(defmethod add-equality-constraint ((nlopt nlopt) (function function) (tol double-float))
  (let ((callback-identifier (add-new-callback nlopt function)))
	(c:add_equality_constraint (ptr nlopt)
							   (cffi:get-callback 'constraint-callback)
							   callback-identifier
							   tol)))

(defmethod (setf xtol-rel) ((tol double-float) (nlopt nlopt))
  (c:set_xtol_rel (ptr nlopt) tol))

(defun foreign-darray-to-lisp (ptr dimension)
  (let ((arr (make-array dimension :element-type 'double-float)))
	(loop for i from 0 below dimension do
		 (setf (aref arr i) (cffi:mem-aref ptr :double i)))
	arr))

(defmethod optimize-nlp ((nlopt nlopt) &optional (initial-x nil x-p))
  (if x-p
	  (progn (assert (= (length initial-x) (dimension nlopt)))
			 (assert (eql (array-element-type initial-x) 'double-float)))
	  (progn
		;; (setf initial-x (cffi:foreign-alloc :double
		;; 									:count (dimension nlopt)
		;;									:initial-element 0d0))))
		(setf initial-x (make-array (dimension nlopt) :initial-element 0d0))))
  (let ((f-value (cffi:foreign-alloc :double))
		(*nlopt-instance* nlopt))
	(declare (special *nlopt-instance*))
	(cffi:with-pointer-to-vector-data (ptr initial-x)
	  (c:optimize_nlp (ptr nlopt) ptr f-value)
	  (let (;;(x (foreign-darray-to-lisp initial-x (dimension nlopt)))
			(f (cffi:mem-ref f-value :double)))
		;;(cffi:foreign-free initial-x)
		(cffi:foreign-free f-value)
		(values initial-x f)))))
  
(defun test ()
  (let ((nlopt (create :nlopt_ld_mma 1)))
	(setf (lower-bounds nlopt) (darray 0d0))
	(setf (min-objective nlopt) (lambda (x grad nlopt)
								  (declare (ignore nlopt))
								  (print (dref x 0))
								  (when grad
									(setf (dref grad 0) (* 2 (dref x 0))))
								  (expt (dref x 0) 2)))
	(setf (xtol-rel nlopt) 1d-4)
	(optimize-nlp nlopt (darray 10d0))))
	

	
