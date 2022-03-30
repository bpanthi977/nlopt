(in-package :nlopt.cffi)

(cl:export 'nlopt_algorithm)
(cffi:defcenum nlopt_algorithm
  (:NLOPT_GN_DIRECT #.0)
  :NLOPT_GN_DIRECT_L
  :NLOPT_GN_DIRECT_L_RAND
  :NLOPT_GN_DIRECT_NOSCAL
  :NLOPT_GN_DIRECT_L_NOSCAL
  :NLOPT_GN_DIRECT_L_RAND_NOSCAL
  :NLOPT_GN_ORIG_DIRECT
  :NLOPT_GN_ORIG_DIRECT_L
  :NLOPT_GD_STOGO
  :NLOPT_GD_STOGO_RAND
  :NLOPT_LD_LBFGS_NOCEDAL
  :NLOPT_LD_LBFGS
  :NLOPT_LN_PRAXIS
  :NLOPT_LD_VAR1
  :NLOPT_LD_VAR2
  :NLOPT_LD_TNEWTON
  :NLOPT_LD_TNEWTON_RESTART
  :NLOPT_LD_TNEWTON_PRECOND
  :NLOPT_LD_TNEWTON_PRECOND_RESTART
  :NLOPT_GN_CRS2_LM
  :NLOPT_GN_MLSL
  :NLOPT_GD_MLSL
  :NLOPT_GN_MLSL_LDS
  :NLOPT_GD_MLSL_LDS
  :NLOPT_LD_MMA
  :NLOPT_LN_COBYLA
  :NLOPT_LN_NEWUOA
  :NLOPT_LN_NEWUOA_BOUND
  :NLOPT_LN_NELDERMEAD
  :NLOPT_LN_SBPLX
  :NLOPT_LN_AUGLAG
  :NLOPT_LD_AUGLAG
  :NLOPT_LN_AUGLAG_EQ
  :NLOPT_LD_AUGLAG_EQ
  :NLOPT_LN_BOBYQA
  :NLOPT_GN_ISRES
  :NLOPT_AUGLAG
  :NLOPT_AUGLAG_EQ
  :NLOPT_G_MLSL
  :NLOPT_G_MLSL_LDS
  :NLOPT_LD_SLSQP
  :NLOPT_LD_CCSAQ
  :NLOPT_GN_ESCH
  :NLOPT_NUM_ALGORITHMS)

(export 'algorithm_name)
(cffi:defcfun ("nlopt_algorithm_name" algorithm_name) :string
  (a nlopt_algorithm))

(export 'nlopt_result)
(cffi:defcenum nlopt_result
  (:NLOPT_FAILURE #.-1)
  (:NLOPT_INVALID_ARGS #.-2)
  (:NLOPT_OUT_OF_MEMORY #.-3)
  (:NLOPT_ROUNDOFF_LIMITED #.-4)
  (:NLOPT_FORCED_STOP #.-5)
  (:NLOPT_SUCCESS #.1)
  (:NLOPT_STOPVAL_REACHED #.2)
  (:NLOPT_FTOL_REACHED #.3)
  (:NLOPT_XTOL_REACHED #.4)
  (:NLOPT_MAXEVAL_REACHED #.5)
  (:NLOPT_MAXTIME_REACHED #.6))

(export 'result_to_string)
(cffi:defcfun ("nlopt_result_to_string" result_to_string) :string
  (a nlopt_result))

(export 'result_from_string)
(cffi:defcfun ("nlopt_result_from_string" result_from_string) nlopt_result
  (name :string))

(export 'srand)
(cffi:defcfun ("nlopt_srand" srand) :void
  (seed :unsigned-long))

(export 'srand_time)
(cffi:defcfun ("nlopt_srand_time" srand_time) :void)

(export 'version)
(cffi:defcfun ("nlopt_version" version) :void
  (major :pointer)
  (minor :pointer)
  (bugfix :pointer))

(export 'create)
(cffi:defcfun ("nlopt_create" create) :pointer
  (algorithm nlopt_algorithm)
  (n :unsigned-int))

(export 'destroy)
(cffi:defcfun ("nlopt_destroy" destroy) :void
  (opt :pointer))

(export 'copy)
(cffi:defcfun ("nlopt_copy" copy) :pointer
  (opt :pointer))

(export 'optimize_nlp)
(cffi:defcfun ("nlopt_optimize" optimize_nlp) nlopt_result
  (opt :pointer)
  (x :pointer)
  (opt_f :pointer))

(export 'set_min_objective)
(cffi:defcfun ("nlopt_set_min_objective" set_min_objective) nlopt_result
  (opt :pointer)
  (f :pointer)
  (f_data :pointer))

(export 'set_max_objective)
(cffi:defcfun ("nlopt_set_max_objective" set_max_objective) nlopt_result
  (opt :pointer)
  (f :pointer)
  (f_data :pointer))

(export 'set_precond_min_objective)
(cffi:defcfun ("nlopt_set_precond_min_objective" set_precond_min_objective) nlopt_result
  (opt :pointer)
  (f :pointer)
  (pre :pointer)
  (f_data :pointer))

(export 'set_precond_max_objective)
(cffi:defcfun ("nlopt_set_precond_max_objective" set_precond_max_objective) nlopt_result
  (opt :pointer)
  (f :pointer)
  (pre :pointer)
  (f_data :pointer))

(export 'get_algorithm)
(cffi:defcfun ("nlopt_get_algorithm" get_algorithm) nlopt_algorithm
  (opt :pointer))

(export 'get_dimension)
(cffi:defcfun ("nlopt_get_dimension" get_dimension) :unsigned-int
  (opt :pointer))

(export 'set_lower_bounds)
(cffi:defcfun ("nlopt_set_lower_bounds" set_lower_bounds) nlopt_result
  (opt :pointer)
  (lb :pointer))

(export 'set_lower_bounds1)
(cffi:defcfun ("nlopt_set_lower_bounds1" set_lower_bounds1) nlopt_result
  (opt :pointer)
  (lb :double))

(export 'set_lower_bound)
(cffi:defcfun ("nlopt_set_lower_bound" set_lower_bound) nlopt_result
  (opt :pointer)
  (i :int)
  (lb :double))


(export 'set_upper_bound)
(cffi:defcfun ("nlopt_set_upper_bound" set_upper_bound) nlopt_result
  (opt :pointer)
  (i :int)
  (lb :double))

(export 'get_lower_bounds)
(cffi:defcfun ("nlopt_get_lower_bounds" get_lower_bounds) nlopt_result
  (opt :pointer)
  (lb :pointer))

(export 'set_upper_bounds)
(cffi:defcfun ("nlopt_set_upper_bounds" set_upper_bounds) nlopt_result
  (opt :pointer)
  (ub :pointer))

(export 'set_upper_bounds1)
(cffi:defcfun ("nlopt_set_upper_bounds1" set_upper_bounds1) nlopt_result
  (opt :pointer)
  (ub :double))

(export 'get_upper_bounds)
(cffi:defcfun ("nlopt_get_upper_bounds" get_upper_bounds) nlopt_result
  (opt :pointer)
  (ub :pointer))

(export 'remove_inequality_constraints)
(cffi:defcfun ("nlopt_remove_inequality_constraints" remove_inequality_constraints) nlopt_result
  (opt :pointer))

(export 'add_inequality_constraint)
(cffi:defcfun ("nlopt_add_inequality_constraint" add_inequality_constraint) nlopt_result
  (opt :pointer)
  (fc :pointer)
  (fc_data :pointer)
  (tol :double))

(export 'add_precond_inequality_constraint)
(cffi:defcfun ("nlopt_add_precond_inequality_constraint" add_precond_inequality_constraint) nlopt_result
  (opt :pointer)
  (fc :pointer)
  (pre :pointer)
  (fc_data :pointer)
  (tol :double))

(export 'add_inequality_mconstraint)
(cffi:defcfun ("nlopt_add_inequality_mconstraint" add_inequality_mconstraint) nlopt_result
  (opt :pointer)
  (m :unsigned-int)
  (fc :pointer)
  (fc_data :pointer)
  (tol :pointer))

(export 'remove_equality_constraints)
(cffi:defcfun ("nlopt_remove_equality_constraints" remove_equality_constraints) nlopt_result
  (opt :pointer))

(export 'add_equality_constraint)
(cffi:defcfun ("nlopt_add_equality_constraint" add_equality_constraint) nlopt_result
  (opt :pointer)
  (h :pointer)
  (h_data :pointer)
  (tol :double))

(export 'add_precond_equality_constraint)
(cffi:defcfun ("nlopt_add_precond_equality_constraint" add_precond_equality_constraint) nlopt_result
  (opt :pointer)
  (h :pointer)
  (pre :pointer)
  (h_data :pointer)
  (tol :double))

(export 'add_equality_mconstraint)
(cffi:defcfun ("nlopt_add_equality_mconstraint" add_equality_mconstraint) nlopt_result
  (opt :pointer)
  (m :unsigned-int)
  (h :pointer)
  (h_data :pointer)
  (tol :pointer))

(export 'set_stopval)
(cffi:defcfun ("nlopt_set_stopval" set_stopval) nlopt_result
  (opt :pointer)
  (stopval :double))

(export 'get_stopval)
(cffi:defcfun ("nlopt_get_stopval" get_stopval) :double
  (opt :pointer))

(export 'set_ftol_rel)
(cffi:defcfun ("nlopt_set_ftol_rel" set_ftol_rel) nlopt_result
  (opt :pointer)
  (tol :double))

(export 'get_ftol_rel)
(cffi:defcfun ("nlopt_get_ftol_rel" get_ftol_rel) :double
  (opt :pointer))

(export 'set_ftol_abs)
(cffi:defcfun ("nlopt_set_ftol_abs" set_ftol_abs) nlopt_result
  (opt :pointer)
  (tol :double))

(export 'get_ftol_abs)
(cffi:defcfun ("nlopt_get_ftol_abs" get_ftol_abs) :double
  (opt :pointer))

(export 'set_xtol_rel)
(cffi:defcfun ("nlopt_set_xtol_rel" set_xtol_rel) nlopt_result
  (opt :pointer)
  (tol :double))

(export 'get_xtol_rel)
(cffi:defcfun ("nlopt_get_xtol_rel" get_xtol_rel) :double
  (opt :pointer))

(export 'set_x_weights)
(cffi:defcfun ("nlopt_set_x_weights" set_x_weights) nlopt_result
  (opt :pointer)
  (w :pointer))

(export 'set_x_weights1)
(cffi:defcfun ("nlopt_set_x_weights1" set_x_weights1) nlopt_result
  (opt :pointer)
  (w :double))

(export 'get_x_weights)
(cffi:defcfun ("nlopt_get_x_weights" get_x_weights) nlopt_result
  (opt :pointer)
  (w :pointer))


(export 'set_xtol_abs1)
(cffi:defcfun ("nlopt_set_xtol_abs1" set_xtol_abs1) nlopt_result
  (opt :pointer)
  (tol :double))

(export 'set_xtol_abs)
(cffi:defcfun ("nlopt_set_xtol_abs" set_xtol_abs) nlopt_result
  (opt :pointer)
  (tol :pointer))

(export 'get_xtol_abs)
(cffi:defcfun ("nlopt_get_xtol_abs" get_xtol_abs) nlopt_result
  (opt :pointer)
  (tol :pointer))

(export 'set_maxeval)
(cffi:defcfun ("nlopt_set_maxeval" set_maxeval) nlopt_result
  (opt :pointer)
  (maxeval :int))

(export 'get_maxeval)
(cffi:defcfun ("nlopt_get_maxeval" get_maxeval) :int
  (opt :pointer))

(export 'set_maxtime)
(cffi:defcfun ("nlopt_set_maxtime" set_maxtime) nlopt_result
  (opt :pointer)
  (maxtime :double))

(export 'get_maxtime)
(cffi:defcfun ("nlopt_get_maxtime" get_maxtime) :double
  (opt :pointer))

(export 'get_numevals)
(cffi:defcfun ("nlopt_get_numevals" get_numevals) :double
  (opt :pointer))

(export 'force_stop)
(cffi:defcfun ("nlopt_force_stop" force_stop) nlopt_result
  (opt :pointer))

(export 'set_force_stop)
(cffi:defcfun ("nlopt_set_force_stop" set_force_stop) nlopt_result
  (opt :pointer)
  (val :int))

(export 'get_force_stop)
(cffi:defcfun ("nlopt_get_force_stop" get_force_stop) :int
  (opt :pointer))

(export 'set_local_optimizer)
(cffi:defcfun ("nlopt_set_local_optimizer" set_local_optimizer) nlopt_result
  (opt :pointer)
  (local_opt :pointer))

(export 'set_population)
(cffi:defcfun ("nlopt_set_population" set_population) nlopt_result
  (opt :pointer)
  (pop :unsigned-int))

(export 'get_population)
(cffi:defcfun ("nlopt_get_population" get_population) :unsigned-int
  (opt :pointer))

(export 'set_vector_storage)
(cffi:defcfun ("nlopt_set_vector_storage" set_vector_storage) nlopt_result
  (opt :pointer)
  (dim :unsigned-int))

(export 'get_vector_storage)
(cffi:defcfun ("nlopt_get_vector_storage" get_vector_storage) :unsigned-int
  (opt :pointer))

(export 'set_default_initial_step)
(cffi:defcfun ("nlopt_set_default_initial_step" set_default_initial_step) nlopt_result
  (opt :pointer)
  (x :pointer))

(export 'set_initial_step)
(cffi:defcfun ("nlopt_set_initial_step" set_initial_step) nlopt_result
  (opt :pointer)
  (dx :pointer))

(export 'set_initial_step1)
(cffi:defcfun ("nlopt_set_initial_step1" set_initial_step1) nlopt_result
  (opt :pointer)
  (dx :double))

(export 'get_initial_step)
(cffi:defcfun ("nlopt_get_initial_step" get_initial_step) nlopt_result
  (opt :pointer)
  (x :pointer)
  (dx :pointer))

(export 'set_munge)
(cffi:defcfun ("nlopt_set_munge" set_munge) :void
  (opt :pointer)
  (munge_on_destroy :pointer)
  (munge_on_copy :pointer))

(export 'munge_data)
(cffi:defcfun ("nlopt_munge_data" munge_data) :void
  (opt :pointer)
  (munge :pointer)
  (data :pointer))

(export 'minimize)
(cffi:defcfun ("nlopt_minimize" minimize) nlopt_result
  (algorithm nlopt_algorithm)
  (n :int)
  (f :pointer)
  (f_data :pointer)
  (lb :pointer)
  (ub :pointer)
  (x :pointer)
  (minf :pointer)
  (minf_max :double)
  (ftol_rel :double)
  (ftol_abs :double)
  (xtol_rel :double)
  (xtol_abs :pointer)
  (maxeval :int)
  (maxtime :double))

(export 'minimize_constrained)
(cffi:defcfun ("nlopt_minimize_constrained" minimize_constrained) nlopt_result
  (algorithm nlopt_algorithm)
  (n :int)
  (f :pointer)
  (f_data :pointer)
  (m :int)
  (fc :pointer)
  (fc_data :pointer)
  (fc_datum_size :pointer)
  (lb :pointer)
  (ub :pointer)
  (x :pointer)
  (minf :pointer)
  (minf_max :double)
  (ftol_rel :double)
  (ftol_abs :double)
  (xtol_rel :double)
  (xtol_abs :pointer)
  (maxeval :int)
  (maxtime :double))

(export 'minimize_econstrained)
(cffi:defcfun ("nlopt_minimize_econstrained" minimize_econstrained) nlopt_result
  (algorithm nlopt_algorithm)
  (n :int)
  (f :pointer)
  (f_data :pointer)
  (m :int)
  (fc :pointer)
  (fc_data :pointer)
  (fc_datum_size :pointer)
  (p :int)
  (h :pointer)
  (h_data :pointer)
  (h_datum_size :pointer)
  (lb :pointer)
  (ub :pointer)
  (x :pointer)
  (minf :pointer)
  (minf_max :double)
  (ftol_rel :double)
  (ftol_abs :double)
  (xtol_rel :double)
  (xtol_abs :pointer)
  (htol_rel :double)
  (htol_abs :double)
  (maxeval :int)
  (maxtime :double))

(export 'get_local_search_algorithm)
(cffi:defcfun ("nlopt_get_local_search_algorithm" get_local_search_algorithm) :void
  (deriv :pointer)
  (nonderiv :pointer)
  (maxeval :pointer))

(export 'set_local_search_algorithm)
(cffi:defcfun ("nlopt_set_local_search_algorithm" set_local_search_algorithm) :void
  (deriv nlopt_algorithm)
  (nonderiv nlopt_algorithm)
  (maxeval :int))

(export 'nlopt_get_stochastic_population)
(cffi:defcfun ("nlopt_get_stochastic_population" nlopt_get_stochastic_population) :int)


(export 'nlopt_set_stochastic_population)
(cffi:defcfun ("nlopt_set_stochastic_population" nlopt_set_stochastic_population) :void
  (pop :int))

;;;
;;;;;;;;;; TEST
;;;


;; (defvar *counts* 0)

;; (cffi:defcallback myfunc :double ((n :unsigned-int) (x :pointer) (grad :pointer) (my_func_data :pointer))
;;   (incf *counts*)
;;   (let ((sqrtx1 (sqrt (cffi:mem-aref x :double 1))))
;;     ;; Set objective gradient when it isn't null
;;      (print (list (cffi:mem-aref x :double 0) (cffi:mem-aref x :double 1)) )
;;     (when (not (cffi:null-pointer-p grad))
;;       (setf (cffi:mem-aref grad :double 0) 0.0d0
;;                      (cffi:mem-aref grad :double 1) (/ 0.5d0 sqrtx1)))
;;     ;; Return objective function value
;;     sqrtx1
;;      1.0d0))

;; (cffi:defcstruct my_constraint_data
;;   (a :double)
;;   (b :double))

;; (cffi:defcallback myconstraint :double ((n :unsigned-int) (x :pointer) (grad :pointer) (data :pointer))
;;   (cffi:with-foreign-slots ((a b) data (:struct my_constraint_data))
;;      (print "conste")
;;     (let ((x0 (cffi:mem-aref x :double 0))
;;           (x1 (cffi:mem-aref x :double 1)))
;;       ;; Set constraint gradient when it isn't null
;;       (when (not (cffi:null-pointer-p grad))
;;         (setf (cffi:mem-aref grad :double 0) (* 3 a (expt (+ (* a x0) b) 2))
;;                        (cffi:mem-aref grad :double 1) -1d0))
;;       ;; Return constraint value
;;       (print (- (expt (+ (* a x0) b) 3) x1)))))



;; (defun test (&optional (alg :NLOPT_LD_MMA))
;;   (let ((opt (nlopt_create (cffi:foreign-enum-value 'nlopt_algorithm alg) 2))
;;              (data-type '(:struct my_constraint_data)))
;;     (cffi:with-foreign-objects ((lb :double 2)
;;                                                              (data0 data-type)
;;                                                              (data1 data-type)
;;                                                              (x :double 2)
;;                                                              (minf :double))
;;       (setf (cffi:mem-aref lb :double 0) -110d0)
;;       (setf (cffi:mem-aref lb :double 1) 0.001d0)
;;       (nlopt_set_lower_bounds opt lb)

;;       (nlopt_set_min_objective opt (cffi:get-callback 'myfunc) (cffi:null-pointer))
;;       (cffi:with-foreign-slots ((a b) data0 (:struct my_constraint_data))
;;              (setf a 2d0 b 0d0))
;;       (cffi:with-foreign-slots ((a b) data1 (:struct my_constraint_data))
;;              (setf a -1d0 b 1d0))

;;       (nlopt_add_equality_constraint opt (cffi:get-callback 'myconstraint) data0 1d-2)
;;       (nlopt_add_equality_constraint opt (cffi:get-callback 'myconstraint) data1 1d-2)
;;       (nlopt_set_xtol_rel opt 1d-4)
;;       (setf (cffi:mem-aref x :double 0) 1.234d0
;;                      (cffi:mem-aref x :double 1) 5.678d0)
;;       (setf *counts* 0)
;;        (print "optimizing")
;;       (nlopt_optimize opt x minf)
;;       (format t "found minimum in ~a steps~&" *counts*)
;;       (format t "found minimum at (f ~a ~a) = ~a~&" (cffi:mem-aref x :double 0) (cffi:mem-aref x :double 1) (cffi:mem-ref minf :double))
;;       (nlopt_destroy opt))))
