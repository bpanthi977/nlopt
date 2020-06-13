;;;; nlopt.lisp

(cl:in-package #:nlopt)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :c :nlopt.cffi))


;;; Library Loading 
(cffi:define-foreign-library nlopt
  (:windows "libnlopt.dll")
  (:linux "libnlopt.so"))

										; Library file may be copied to project directory 
(pushnew (asdf:system-source-directory :nlopt) cffi:*foreign-library-directories*)
(cffi:load-foreign-library 'nlopt)

;;; nlopt object 
(defclass nlopt ()
  ((ptr :initarg :ptr :accessor ptr
		:documentation "Pointer to nlopt object")
   (dimension :initarg :dimension :reader dimension
			  :documentation "Number of variables")
   (objective :reader objective
			  :documentation "Objective function")
   (callbacks :accessor callbacks :initform nil
			  :documentation "Callbacks for different constraints")
   (preconditioner :reader preconditioner
				   :documentation "Preconditioner used with `CCSAQ' algorithm")
   (identifiers :accessor identifiers :initform nil
				:documentation "Actually the nlopt c library is passed a single callback which 
selects which lisp callback to use. The identifer foreign struct is used to select/identify callbacks")))

;;;;;
;;; Mechanism for callbacks
;;;;;

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
  "Callback function for objective"
  (declare (ignore n user_data))
  (funcall (objective *nlopt-instance*)
		   x
		   (unless (cffi:null-pointer-p grad)
			 grad)
		   *nlopt-instance*))

(cffi:defcallback preconditioner-callback :double
	((n :unsigned-int) (x :pointer) (v :pointer) (vpre :pointer) (user_data :pointer))
  "Callback function for objective"
  (declare (ignore user_data))
  (let ((r (funcall (preconditioner *nlopt-instance*)
					x
					v 
					*nlopt-instance*)))
	(assert (= (length r) n))
	(setf-doubles vpre r)))


(cffi:defcallback constraint-callback :double
	((dimension :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  "Callback function for constraints"
  (declare (ignore dimension))
  (cffi:with-foreign-slots ((n) data (:struct callback-identifier))
	(funcall (nth n (callbacks *nlopt-instance*))
			 x
			 (unless (cffi:null-pointer-p grad)
			   grad)
			 *nlopt-instance*)))

(cffi:defcallback mconstraint-callback :void
	((m :unsigned-int) (result :pointer) (dimension :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
  "Callback function for mconstraints (multiple constraints)"
  (declare (ignore dimension))
  (cffi:with-foreign-slots ((n) data (:struct callback-identifier))
	(let ((r (funcall (nth n (callbacks *nlopt-instance*))
					  m
					  x
					  (unless (cffi:null-pointer-p grad)
						grad)
					  *nlopt-instance*)))
	  (assert (= (length r) m))
	  (setf-doubles result r))))

;;;;;
;;; Bindings
;;;;;

(defun algorithms ()
  "List of algorithms available"
  (cffi:foreign-enum-keyword-list 'c:nlopt_algorithm))

(defun algorithm (nlopt)
  "Get algorithm used; Returns a keyword"
  (c:get_algorithm (ptr nlopt)))

(defun algorithm-name (algorithm)
  "Human readable name of `algorithm'"
  (c:algorithm_name algorithm)) 

(defun create (algorithm dimension)
  "Create an Non-linear optimization object object
`algorithm' is one of the `(algorithms)'
`dimension' is number of parameters in the optimization problem"
  (assert  (and (integerp dimension)
				(> dimension 0))
		   (dimension))
  (assert (member algorithm (algorithms)) (algorithm))
  (let ((ptr (c:create (cffi:foreign-enum-value 'c:nlopt_algorithm algorithm) dimension)))
	(unless (cffi:null-pointer-p ptr)
	  (let ((nlopt (make-instance 'nlopt
								  :dimension dimension
								  :ptr ptr)))
		(prog1 nlopt
		  (tg:finalize nlopt
					   #'(lambda ()
						   (c:destroy ptr))))))))

(defmethod copy-object ((obj nlopt))
  "Make of copy of nlopt object in lisp side only
You might want to use `copy' to make actual copy of object in lisp and c side"
  (let ((o (make-instance 'nlopt
						  :dimension (dimension obj))))
	(with-slots (objective callbacks identifiers) obj
	  (setf (slot-value o 'objective) objective
			(callbacks o) callbacks
			(slot-value o 'identifiers) identifiers))
	o))

(defun copy (nlopt)
  "Make a copy of `nlopt' problem
If you are subclassing the `nlopt' object define a `copy-object' method on your subclass 
that copies all slots (including those of `nlopt')"
  (let* ((foreign-new (c:copy (ptr nlopt)))
		 (lisp-new (copy-object nlopt)))
	(setf (ptr lisp-new) foreign-new)
	(tg:finalize lisp-new
				 #'(lambda ()
					 (c:destroy foreign-new)))
	lisp-new))

;;;;; Setting and Gettting Upper and Lower *Bounds* for parameters

(defun (setf lower-bounds) (bounds nlopt)
  "Set lower bounds for parameters
bounds can be a number, list or array of doubles"
  (etypecase bounds
	(number (c:set_lower_bounds1 (ptr nlopt) bounds))
	(list (assert (= (length bounds) (dimension nlopt)))
	 (c:set_lower_bounds (ptr nlopt) (doubles bounds)))
	(doubles (assert (= (length bounds) (dimension nlopt)))
	 (c:set_lower_bounds (ptr nlopt) bounds))))

(defun (setf upper-bounds) (bounds nlopt)
  "Set upper bounds for parameters
bounds can be a number, list or array of doubles"
  (etypecase bounds
	(number (c:set_upper_bounds1 (ptr nlopt) bounds))
	(list (assert (= (length bounds) (dimension nlopt)))
	 (c:set_upper_bounds (ptr nlopt) (doubles bounds)))
	(doubles (assert (= (length bounds) (dimension nlopt)))
	 (c:set_upper_bounds (ptr nlopt) bounds))))

(defun lower-bounds (nlopt)
  "Get lower bounds for parameters"
  (let ((lb (make-array (dimension nlopt) :element-type 'double-float)))
	(cffi:with-pointer-to-vector-data (lb-ptr lb)
	  (c:get_lower_bounds (ptr nlopt) lb-ptr))
	lb))

(defun upper-bounds (nlopt)
  "Get upper bounds for parameters"
  (let ((ub (make-array (dimension nlopt) :element-type 'double-float)))
	(cffi:with-pointer-to-vector-data (ub-ptr ub)
	  (c:get_lower_bounds (ptr nlopt) ub-ptr))
	ub))

(defun (setf lower-bound) (bound nlopt i)
  (c:set_lower_bound (ptr nlopt) i bound))

(defun lower-bound (nlopt i)
  (svref (lower-bounds nlopt) i))

(defun (setf upper-bound) (bound nlopt i)
  (c:set_upper_bound (ptr nlopt) i bound))

(defun upper-bound (nlopt i)
  (svref (upper-bounds nlopt) i))
;;;;; Set objective function

(defun set-min-objective (nlopt function)
  "Set minimization objective function where 
function(pointer to doubles x, 
         pointer to doubles grad or NULL, 
         nlopt)
returns a double-float"
  (c:set_min_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))

(defun set-max-objective (nlopt function)
  "Set maximazation objective function where 
function(pointer to doubles x, 
         pointer to doubles grad or NULL, 
         nlopt)
returns a double-float"
  (c:set_max_objective (ptr nlopt) (cffi:get-callback 'objective-callback) (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function))

(defun set-precond-min-objective (nlopt function preconditioner)
  "If you know the Hessian (second-derivative) matrix of your objective function,
i.e. the matrix H with Hij=∂2f/∂x_i∂x_j

for an objective f, then in principle this could be used to accelerate local
optimization. In fact, even a reasonable approximation for H could be useful if
it captures information about the largest eigenvalues of H and the corresponding
eigenvectors. Such an approximate Hessian is often called a preconditioner in
the context of iterative solvers, so we adopt that terminology here.

Currently, support for preconditioners in NLopt is somewhat experimental, and is
only used in the NLOPT_LD_CCSAQ algorithm. 

The preconditioner is a function 

pre(pointer to doubles x,
    pointer to doubles v, 
    nlopt)

This function should take a vector v and should compute vpre = H(x) v where H is
an approximate second derivative at x. The CCSAQ algorithm requires that your
matrix H be positive semidefinite, i.e. that it be real-symmetric with
nonnegative eigenvalues."
  (c:set_precond_min_objective (ptr nlopt) (cffi:get-callback 'objective-function)
							   (cffi:get-callback 'preconditioner-callback)
							   (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function
		(slot-value nlopt 'preconditioner) preconditioner))

(defun set-precond-max-objective (nlopt function preconditioner)
  "See documentation for set-precond-min-objective"
  (c:set_precond_max_objective (ptr nlopt) (cffi:get-callback 'objective-function)
							   (cffi:get-callback 'preconditioner-callback)
							   (cffi:null-pointer))
  (setf (slot-value nlopt 'objective) function
		(slot-value nlopt 'preconditioner) preconditioner))


;;;;; Add Constraints

(defun add-equality-constraint (nlopt function &optional (tol 1d-6))
  "Add an equality constaint c(x) = 0
c(pointer to doubles x, pointer to gradient or NULL, nlopt) 
tolerance `tol' is used to check -tol <= c(x) <= tol"
  (let ((callback-identifier (add-new-callback nlopt function)))
	(c:add_equality_constraint (ptr nlopt)
							   (cffi:get-callback 'constraint-callback)
							   callback-identifier
							   tol)
	callback-identifier))


(defun add-inequality-constraint (nlopt function &optional (tol 1d-6))
  "Add an inequality constaint c(x) <= 0
c(pointer to doubles x, pointer to gradient or NULL, nlopt) 
tolerance `tol' is used to check c(x) <= tol"
  (let ((callback-identifier (add-new-callback nlopt function)))
	(c:add_equality_constraint (ptr nlopt)
							   (cffi:get-callback 'constraint-callback)
							   callback-identifier
							   tol)
	callback-identifier))

(defun remove-equality-constraints (nlopt)
  "Remove all equality constraints"
  ;; TODO: remove identifiers, and callbacks from nlopt object too 
  (c:remove_equality_constraints (ptr nlopt)))

(defun remove-inequality-constraints (nlopt)
  "Remove all inequality constraints"
  ;; TODO: remove identifiers, and callbacks from nlopt object too 
  (c:remove_inequality_constraints (ptr nlopt)))

(defmethod add-equality-mconstraint (nlopt m function tol)
  "Add multiple equality constraints c_i(x) = 0; i=1,2,..m 
`function' = `c' should return list of `m' doubles
c(m, 
  pointer to doubles x,
  pointer to gradient(jacobian) `∂c_i(x)/∂x_j' or NULL,  (m rows, n columns)
  nlopt)
`tol' is tolerances as in single constaints, it can be a number, nil, list of tolerance or array of double-floats

In some applications with multiple constraints, it is more convenient to define a single function
 that returns the values (and gradients) of all constraints at once. For example, different constraint
 functions might share computations in some way. Or, if you have a large number of constraints, you may 
wish to compute them in parallel."
  (let ((callback-identifier (add-new-callback nlopt function))
		(tol (etypecase tol
			   (number (doubles-array m tol))
			   (null (cffi:null-pointer))
			   (list (assert (= (length tol) m)) (doubles tol))
			   (doubles tol))))
	(c:add_equality_mconstraint (ptr nlopt)
								m
								(cffi:get-callback 'mconstraint-callback)
								callback-identifier
								tol)))

(defmethod add-inequality-mconstraint (nlopt m function tol)
  "Add multiple inequality constraints c_i(x) = 0; i=1,2,..m 
`function' = `c' should return list of `m' doubles
c(m, 
  pointer to doubles x,
  pointer to gradient(jacobian) `∂c_i(x)/∂x_j',  (m rows, n columns)
  nlopt)
`tol' is tolerances as in single constaints, it can be a number, nil, list of tolerance or array of double-floats

In some applications with multiple constraints, it is more convenient to define a single function
that returns the values (and gradients) of all constraints at once. For example, different constraint
functions might share computations in some way. Or, if you have a large number of constraints, you may 
wish to compute them in parallel."
  (let ((callback-identifier (add-new-callback nlopt function))
		(tol (etypecase tol
			   (number (doubles-array m tol))
			   (null (cffi:null-pointer))
			   (list (assert (= (length tol) m)) (doubles tol))
			   (doubles tol))))
	(c:add_inequality_mconstraint (ptr nlopt)
								  m
								  (cffi:get-callback 'mconstraint-callback)
								  callback-identifier
								  tol)))

;;;;; Stopping Criteria

;;; Objective function stopvalue
(defun (setf stopval) (stopval nlopt)
  "Stop when an objective value of at least stopval is found: 
stop minimizing when an objective value <= stopval is found, 
or stop maximizing a value >= stopval is found."
  (c:set_stopval (ptr nlopt) stopval))

(defun stopval (nlopt)
  "Get the current stopping value for objective function"
  (c:get_stopval (ptr nlopt)))

;;; Objective function update tolerances
(defun (setf ftol-rel) (tol nlopt)
  "Set relative tolerance on function value: stop when an optimization step 
 (or an estimate of the optimum) changes the objective function value by 
less than `tol' multiplied by the absolute value of the function value. 
 (If there is any chance that your optimum function value is close to zero, 
you might want to set an absolute tolerance with (setf (ftol-abs nlopt)) as well.)

Criterion is disabled if `tol' is non-positive."
  (declare (type double-float tol))
  (c:set_ftol_rel (ptr nlopt) tol))

(defun ftol-rel (nlopt)
  "Get relative tolerance in changes in objective function value"
  (c:get_ftol_rel (ptr nlopt)))

(defun (setf ftol-abs) (tol nlopt)
  "Set relative tolerance on function value: stop when an optimization step 
 (or an estimate of the optimum) changes the objective function value by 
less than `tol' multiplied by the absolute value of the function value.

Criterion is disabled if `tol' is non-positive"
  (declare (type double-float tol))
  (c:set_ftol_abs (ptr nlopt) tol))

;;; Parameter update tolerances
(defun (setf xtol-rel) (tol nlopt)
  "Set relative tolerance on optimization parameters: stop when an optimization step 
 (or an estimate of the optimum) causes a relative change the parameters `x' by less than `tol', 
i.e. ∥Δx∥_w <tol * ∥x∥_w as measured by a weighted L₁ norm ∥x∥w=∑_i w_i * |x_i|, 
where the weights w_i default to 1. (If there is any chance that the optimal ∥x∥ is close to zero, 
you might want to set an absolute tolerance with `set-xtol-abs' as well.)

Criterion is disabled if `tol' is non-positive."
  (c:set_xtol_rel (ptr nlopt) tol))

(defun xtol-rel (nlopt)
  "Get relative tolerance in parameters `x'"
  (c:get_xtol_rel (ptr nlopt)))

(defun (setf x-weights) (weights nlopt)
  "Set weights `w_i' for relalative tolerance in updates of parameters `x'
weights can be a single number, list, or array of doubles"
  (etypecase weights
	(number (c:set_x_weights1 (ptr nlopt) (coerce weights 'double-float)))
	(list (assert (= (dimension nlopt) (length weights)))
	 (c:set_x_weights (ptr nlopt) (doubles weights)))
	(doubles (assert (= (dimension nlopt) (length weights)))
	 (c:set_x_weights (ptr nlopt) weights))))

(defun x-weights (nlopt)
  "Get the weights `w_i' set for relative tolerance in updates of parameters `x'"
  (let ((a (doubles-array (dimension nlopt) 0d0)))
	(cffi:with-pointer-to-vector-data (ptr a)
	  (c:get_x_weights (ptr nlopt) ptr))
	a))

(defun (setf xtol-abs) (nlopt tolerances)
  "Set absolute tolerances on optimization parameters. Stop when an optimization step 
 (or an estimate of the optimum) changes every parameter x[i] by less than tol[i]. 
Note that since nlopt creates a copy of tolerances array subsequent changes to 
the caller's tolerances have no effect on opt. 
`tolerances' can be a number, an list or array of doubles of size n = (dimension nlopt)
Criterion is disabled if `tolerances' is non-positive."
  (etypecase tolerances
	(number (c:set_xtol_abs1 (ptr nlopt) (coerce tolerances 'double-float)))
	(list (assert (= (dimension nlopt) (length tolerances)))
	 (c:set_xtol_abs (ptr nlopt) (doubles tolerances)))
	(doubles (assert (= (dimension nlopt) (length tolerances)))
	 (c:set_xtol_abs (ptr nlopt) tolerances))))


(defun xtol_abs (nlopt)
  "Get the stopping tolerances set for updates in parameters `x'"
  (let ((a (doubles-array (dimension nlopt) 0d0)))
	(cffi:with-pointer-to-vector-data (ptr a)
	  (c:get_xtol_abs (ptr nlopt) ptr))
	a))

(defun (setf maxeval) (maxeval nlopt)
  "Stop when the number of function evaluations exceeds `maxeval'
This is not a strict maximum: the number of function evaluations may exceed `maxeval' slightly, 
depending upon the algorithm.
`maxeval' is an integer
 Criterion is disabled if `maxeval' is non-positive"
  (c:set_maxeval (ptr nlopt) maxeval))

(defun maxeval (nlopt)
  "Get maximum number of function evaluations"
  (c:get_maxeval (ptr nlopt)))

(defun (setf maxtime) (time nlopt)
  "Stop when the optimization time (in seconds) exceeds maxtime.
This is not a strict maximum: the time may exceed maxtime slightly,
 depending upon the algorithm and on how slow your function evaluation is.
`time' is a double-float
Criterion is disabled if maxtime is non-positive."
  (c:set_maxtime (ptr nlopt) time))

(defun maxtime (nlopt)
  "Get maximum optimization time allowed (in seconds)"
  (c:get_maxtime (ptr nlopt)))

(defun numevals (nlopt)
  "Number of function evaluations used to solve the opt problem"
  (c:get_numevals (ptr nlopt)))

;;; Forced termination
(defun force-stop (nlopt)
  "In certain cases, the caller may wish to force the optimization to halt, for some reason unknown to NLopt. 
For example, if the user presses Ctrl-C, or there is an error of some sort in the objective function. 
In this case, it is possible to tell NLopt to halt the optimization gracefully, returning the best point
found so far, by calling the this function from within your objective or constraint functions"
  (c:force_stop (ptr nlopt)))

(defun (setf force-stop-value) (val nlopt)
  "Set a reason (integer value) for force stopping that can be later retrieved using `force-stop-value'"
  (c:set_force_stop (ptr nlopt) val))

(defun force-stop-value (nlopt)
  "Returns `value' set using `(setf (force-stop-value nlopt) value)'"
  (c:get_force_stop (ptr nlopt)))

;;;;; Optimization

(defun default-initial-x (nlopt)
  (make-array (dimension nlopt)
			  :initial-element 0d0
			  :element-type 'double-float))

(defmethod optimize-nlp ((nlopt nlopt) &optional (x nil))
  "Optimize the NonLinear Optimization problem `nlopt' with initial parameters initial-x"
  (setf x (etypecase x
			  (null (default-initial-x nlopt))
			  (list (assert (= (length x) (dimension nlopt)))
			   (doubles x))
			  (doubles (assert (= (length x) (dimension nlopt)))
			   x)))
  (cffi:with-foreign-object (f-value :double)
	(let ((*nlopt-instance* nlopt))
	  (cffi:with-pointer-to-vector-data (ptr x)
		(let ((result (c:optimize_nlp (ptr nlopt) ptr f-value)))
		  (values x (cffi:mem-ref f-value :double) result))))))

(defun result-description (result)
  "Gives description for halting (forcefully or sucessfully) of the optimization problem
 (result-description (nth-value 2 (optimize-nlp nlopt)))"
  (ecase result
	(:NLOPT_SUCCESS "Generic success return value.")
	(:NLOPT_STOPVAL_REACHED "Optimization stopped because stopval (above) was reached.")
	(:NLOPT_FTOL_REACHED "Optimization stopped because ftol_rel or ftol_abs (above) was reached.")
	(:NLOPT_XTOL_REACHED "Optimization stopped because xtol_rel or xtol_abs (above) was reached.")
	(:NLOPT_MAXEVAL_REACHED "Optimization stopped because maxeval (above) was reached.")
	(:NLOPT_MAXTIME_REACHED "Optimization stopped because maxtime (above) was reached.")
	(:NLOPT_FAILURE "Generic failure code.")
	(:NLOPT_INVALID_ARGS "Invalid arguments (e.g. lower bounds are bigger than upper bounds, an unknown algorithm was specified, etcetera).")
	(:NLOPT_OUT_OF_MEMORY "Ran out of memory.")
	(:NLOPT_ROUNDOFF_LIMITED "Halted because roundoff errors limited progress. (In this case, the optimization still typically returns a useful result.)")
	(:NLOPT_FORCED_STOP "Halted because of a forced termination: the user called nlopt_force_stop(opt) on the optimization’s nlopt_opt object opt from the user’s objective function or constraints.")))


(defun  set-local-optimizer (nlopt local-nlopt)
  "Some of the algorithms, especially MLSL and AUGLAG, use a different optimization
algorithm as a subroutine, typically for local optimization. You can change the
local search algorithm and its tolerances by calling: 

 (setf (local-optimizer nlopt) local-nlopt)

Here, `local-nlopt' is another nlopt object whose parameters are used to
determine the local search algorithm, its stopping criteria, and other algorithm
parameters. (However, the objective function, bounds, and nonlinear-constraint
parameters of local_opt are ignored.) The dimension `n' of `local-nlopt' must match 
that of `nlopt'.

This function makes a copy of the `local-nlopt' object, so local-optimizer must be 
assigned to `nlopt' after all criterias are set."
  (c:set_local_optimizer (ptr nlopt) (ptr local-nlopt)))

(defun (setf initial-step) (dx nlopt)
  "`dx' is a number, list or array of doubles of length n (the dimension of the
problem) containing the (nonzero) initial step size for each component of the
optimization parameters x. If you pass nil for dx, then NLopt will use its
heuristics to determine the initial step size.

For derivative-free local-optimization algorithms, the optimizer must somehow
decide on some initial step size to perturb x by when it begins the
optimization. This step size should be big enough that the value of the
objective changes significantly, but not too big if you want to find the local
optimum nearest to x. By default, NLopt chooses this initial step size
heuristically from the bounds, tolerances, and other information, but this may
not always be the best choice.
"
  (etypecase dx
	(null (c:set_initial_step (ptr nlopt) (cffi:null-pointer)))
	(number (c:set_initial_step1 (ptr nlopt) (coerce dx 'double-float)))
	(list (c:set_initial_step (ptr nlopt) (doubles dx)))
	(doubles (c:set_initial_step (ptr nlopt) dx))))

(defun initial-step (nlopt x)
  "Get the initial step size
Here, x is the same as the initial guess that you plan to pass to `optimize-nlp'
if you have not set the initial step and NLopt is using its heuristics, its
heuristic step size may depend on the initial x, which is why you must pass it
here."
  (setf x (etypecase x
			(null (default-initial-x nlopt))
			(list (assert (= (length x) (dimension nlopt)))
			 (doubles x))
			(doubles x)))
  (let ((dx (doubles-array (dimension nlopt) 0d0)))
	(cffi:with-pointer-to-vector-data (ptr-dx dx)
	  (cffi:with-pointer-to-vector-data (ptr-x x)
		(c:get_initial_step (ptr nlopt) ptr-x ptr-dx)))
	dx))

(defun set-population (nlopt size)
  "Several of the stochastic search algorithms (e.g., CRS, MLSL, and ISRES) start
by generating some initial `population' of random points x. By default, this
initial population size is chosen heuristically in some algorithm-specific way,
but the initial population can by changed by this function. A `size' of zero implies 
that the heuristic default will be used."
  (c:set_population (ptr nlopt) size))

(defun srand (seed)
  "For stochastic optimization algorithms, we use pseudorandom numbers generated by
the Mersenne Twister algorithm, based on code from Makoto Matsumoto. By default,
the seed for the random numbers is generated from the system time, so that you
will get a different sequence of pseudorandom numbers each time you run your
program. If you want to use a `deterministic' sequence of pseudorandom numbers,
i.e. the same sequence from run to run, you can set the seed
"
  (c:srand seed))

(defun srand-time ()
  "Reset the seed based on the system time 
 (It is called automatically, but call this if you have set a deterministic seed using 
`srand' and want to re-randomize the seed)"
  (c:srand_time))


(defun (setf vector-storage) (M nlopt)
  "Some of the NLopt algorithms are limited-memory `quasi-Newton' algorithms, which
`remember' the gradients from a finite number M of the previous optimization
steps in order to construct an approximate 2nd derivative matrix. The bigger M
is, the more storage the algorithms require, but on the other hand they may
converge faster for larger M. By default, NLopt chooses a heuristic value of M.

Passing M=0 (the default) tells NLopt to use a heuristic value. By default,
NLopt currently sets M to 10 or at most 10 MiB worth of vectors, whichever is
larger."
  (c:set_vector_storage (ptr nlopt) M))

(defun vector-storage (nlopt)
  "Get the size/number of previous optimization gradients storage used in 
limited-memory `quasi-Newton' alogrithms"
  (c:get_vector_storage (ptr nlopt)))

(defun version ()
  "Get underlying NLopt c library versions (major, minor, bugfix)"
  (cffi:with-foreign-objects ((major :int)
							  (minor :int)
							  (bugfix :int))
	(c:version major minor bugfix)
	(values (cffi:mem-ref major :int)
			(cffi:mem-ref minor :int)
			(cffi:mem-ref bugfix :int))))
