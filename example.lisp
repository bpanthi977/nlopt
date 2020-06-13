(in-package :nlopt)

(defun example-simple ()
  "Finds minima of f(x) = (x+3)^2"
  (let ((nlopt (create :nlopt_ld_mma 1)))
	(set-lower-bounds nlopt (darray 0d0))
	(set-min-objective nlopt (lambda (x grad nlopt)
								  (declare (ignore nlopt))
								  (when grad
									(setf (dref grad 0) (* 2 (+ 3d0 (dref x 0)))))
								  (expt (+ 3d0 (dref x 0)) 2)))
	(set-xtol-rel nlopt 1d-4)
	(optimize-nlp nlopt (darray 10d0))))
	
(defun example-mma()
  "The https://nlopt.readthedocs.io/en/latest/NLopt_Tutorial/ problem"
  (let ((nlopt (create :nlopt_ld_mma 2)))
	(set-lower-bounds nlopt (doubles (list -11d0 0.001d0)))
	(set-min-objective nlopt (lambda (x grad nlopt)
							   (declare (ignore nlopt))
							   (let ((sqrtx1 (sqrt (dref x 1))))
								 (when grad
								   (setf-darray grad
												0.0d0
												(/ 0.5d0 sqrtx1)))
								 sqrtx1)))
	(add-inequality-constraint nlopt
							   (lambda (x grad nlopt)
								 (declare (ignore nlopt))
								 (let ((x0 (dref x 0))
									   (x1 (dref x 1)))
								   (setf-darray grad
												(* 6 (expt (* 2 x0) 2))
												-1d0)
								   (- (expt (* 2 x0) 3) x1)))
							   1d-2)
	(add-inequality-constraint nlopt
							   (lambda (x grad nlopt)
								 (declare (ignore nlopt))
								 (let ((x0 (dref x 0))
									   (x1 (dref x 1)))
								   (setf-darray grad
												(* -3 (expt (+ (- x0) 1d0) 2))
												-1d0)
								   (- (expt (+ (- x0) 1) 3) x1)))
							   1d-2)
	(set-xtol-rel nlopt 1d-4)
	(optimize-nlp nlopt (doubles (list 1.234d0 5.67d0)))))

(defun example2 ()
  (let ((nlopt (create :nlopt_ln_cobyla 5)))
	(set-min-objective nlopt
					   (lambda (x grad nlopt)
						 (declare (ignore grad nlopt))
						 (dreffing@ x
						   (+ @0 @1 @2 @3 @4))))
	(setf (lower-bounds nlopt) -10d0)
	(setf (xtol-rel nlopt) 1d-4)
	;; (add-equality-constraint nlopt
	;; 						 (lambda (x grad nlopt)
	;; 						   (declare (ignore grad nlopt))
	;; 						   (dreffing@ x (+ @0 @1))))
	(add-equality-mconstraint nlopt 3
							  (lambda (x grad nlopt)
								(declare (ignore grad nlopt))
								(dreffing@ x
								  (list (+ @0 (* @1 @2))
										(- @3 (/ @4 @2))
										(/ @1 (* @0 @1)))))
							  0.001)
	(optimize-nlopt nlopt 1)))
									  
(defmacro with-nlopt ((nlopt-var algorithm dimension &key (xtol-rel 1d-4) (xtol-abs 1d-4)) &body body)
  `(let ((,nlopt-var (create ,algorithm ,dimension)))
	 (setf (xtol-rel ,nlopt-var) ,xtol-rel)
	 (setf (xtol-abs ,nlopt-var) ,xtol-abs)
   ,@body))

(defun example3 ()
  (with-nlopt (nlopt :nlopt_ln_cobyla 1 :xtol-rel 1d-6 :xtol-abs 1d-7)
	(set-min-objective nlopt (lambda (x grad nlopt)
							   (declare (ignore grad nlopt))
							   (abs (- (exp (- (dref x 0))) (exp 1)))))
	(setf (ftol-ab))
	(optimize-nlopt nlopt 10)))

