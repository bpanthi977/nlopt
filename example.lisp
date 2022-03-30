(in-package :nlopt)

(defun example1 ()
  "Finds minima of f(x) = (x+3)^2"
  (let ((nlopt (create :nlopt_ld_mma 1)))
    (setf (lower-bounds nlopt) '(0d0))
    (set-min-objective nlopt (lambda (x grad nlopt)
                               (declare (ignore nlopt))
                               (when grad
                                 (setf (dref grad 0) (* 2 (+ 3d0 (dref x 0)))))
                               (expt (+ 3d0 (dref x 0)) 2)))
    (setf (xtol-abs nlopt) 1d-4)
    (optimize-nlopt nlopt '(10d0))))

(defun example1-nloptimize ()
  (nloptimize ((x) :initial '(10d0))
    (:minimize (expt x 2))
    (:satisfy (> x 0))
    (setf (xtol-abs *nlopt-instance*) 1d-4)))

(defun example-mma()
  "The https://nlopt.readthedocs.io/en/latest/NLopt_Tutorial/ problem
minimize sqrt(y)
subject to
  y >= (2x)^3
  y >= (1 - x)^3
  y>0"
  (let ((nlopt (create :nlopt_ld_mma 2)))
    (setf (lower-bound nlopt 1) 0d0)
    (set-min-objective nlopt (lambda (x grad nlopt)
                               (declare (ignore nlopt))
                               (let ((sqrtx1 (sqrt (dref x 1))))
                                 (when grad
                                   (setf-doubles grad
                                                 0.0d0
                                                 (/ 0.5d0 sqrtx1)))
                                 sqrtx1)))
    (add-inequality-constraint nlopt
                               (lambda (x grad nlopt)
                                 (declare (ignore nlopt))
                                 (let ((x0 (dref x 0))
                                       (x1 (dref x 1)))
                                   (setf-doubles grad
                                                 (* 6 (expt (* 2 x0) 2))
                                                 -1d0)
                                   (- (expt (* 2 x0) 3) x1)))
                               1d-2)
    (add-inequality-constraint nlopt
                               (lambda (x grad nlopt)
                                 (declare (ignore nlopt))
                                 (let ((x0 (dref x 0))
                                       (x1 (dref x 1)))
                                   (setf-doubles grad
                                                 (* -3 (expt (+ (- x0) 1d0) 2))
                                                 -1d0)
                                   (- (expt (+ (- x0) 1) 3) x1)))
                               1d-2)
    (setf (xtol-rel nlopt) 1d-4)
    (optimize-nlopt nlopt (doubles (list 1.234d0 5.678d0)))))

(defun example2 ()
  "Same problem as (example-mma)
The https://nlopt.readthedocs.io/en/latest/NLopt_Tutorial/ problem
minimize sqrt(y)
subject to
  y >= (2x)^3
  y >= (1 - x)^3
  y>0"
  (nloptimize ((x y)
               :initial '(1.234 5.678))
    (:minimize (sqrt y))
    (:satisfy (>= y (expt (* 2 x) 3))
              (>= y (expt (- 1 x) 3))
              (> y 0))
    (setf (xtol-rel *nlopt-instance*) 1d-4)))

(defun example3 ()
  (let ((nlopt (create :nlopt_ln_cobyla 5)))
    (set-min-objective nlopt
                       (lambda (x grad nlopt)
                         (declare (ignore grad nlopt))
                         (dreffing@ x
                           (+ @0 @1 @2 @3 @4))))
    (setf (lower-bounds nlopt) -10d0)
    (setf (xtol-rel nlopt) 1d-4)
    (add-equality-mconstraint nlopt 3
                              (lambda (x grad nlopt)
                                (declare (ignore grad nlopt))
                                (dreffing@ x
                                  (list (+ @0 (* @1 @2))
                                        (- @3 (/ @4 @2))
                                        (/ @1 (* @0 @1)))))
                              0.001)
    (optimize-nlopt nlopt 1)))
