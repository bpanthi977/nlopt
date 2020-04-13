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
	(set-lower-bounds nlopt (darray -11d0 0.001d0))
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
	(optimize-nlp nlopt (darray 1.234d0 5.67d0))))
