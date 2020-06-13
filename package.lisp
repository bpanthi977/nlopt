;;;; package.lisp

(cl:defpackage #:nlopt.cffi
  (:use #:cl))

(cl:defpackage #:nlopt
  (:use #:cl)
  (:export #:copy
		   #:algorithm
		   #:dimensions
		   #:algorithm-name
		   #:create
		   
		   #:set-min-objective
		   #:set-max-objective
		   #:set-precond-min-objective
		   #:set-precond-max-objective

		   #:add-inequality-constraint
		   #:add-equality-constraint
		   #:remove-inequality
		   #:remove-equality
		   #:add-inequality-mconstraints
		   #:add-equality-mconstraints

		   #:lower-bounds
		   #:upper-bounds
		   #:lower-bound
		   #:upper-bound

		   #:stopval
		   #:ftol-rel
		   #:ftol-abs
		   #:xtol-rel
		   #:x-weights
		   #:xtol-abs
		   #:maxeval
		   #:maxtime
		   #:numevals

		   #:force-stop
		   #:force-stop-value

		   #:optimize-nlopt
		   #:set-local-optimizer

		   #:initial-step
		   #:set-population
		   #:srand
		   #:srand-time
		   #:vector-storage))




