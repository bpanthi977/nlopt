;;;; package.lisp

(defpackage #:nlopt.cffi
  (:use #:cl))

(defpackage #:nlopt
  (:use #:cl)
  (:local-nicknames (:c :nlopt.cffi))
  (:export #:copy
           #:algorithm
           #:dimensions
           #:algorithms
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
           #:nloptimize
           #:set-local-optimizer

           #:initial-step
           #:set-population
           #:srand
           #:srand-time
           #:vector-storage

           #:result-description
           #:version))
