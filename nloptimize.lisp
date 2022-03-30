(in-package :nlopt)

;;;;; High Level nloptimize macro

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filter-visible-vars (vars body &optional sofar)
    "Returns list of `vars' that are seen in `body'
results found are stored in `sofar'"
    (cond ((null body) sofar)
          ((symbolp body)
           (if (and (find body vars)
                    (not (find body sofar)))
               (cons body sofar)
               sofar))
          ((not (listp body)) sofar)
          ((eql (first body) 'quote) sofar)
          (t (dolist (el (rest body) sofar)
               (setf sofar (filter-visible-vars vars el sofar))))))

  (defun no-visible-vars? (vars exp)
    ;; TODO write a more efficient function
    (not (filter-visible-vars vars exp)))

  (defun %with-nlopt-lambda (vars expression)
    (alexandria:with-gensyms (x grad nlopt)
      (let* ((found (filter-visible-vars vars expression))
             (bindings (loop for v in found
                             collect (list v (list 'dref x (position v vars))))))
        (if bindings
            `(lambda (,x ,grad ,nlopt)
               (declare (ignore ,grad ,nlopt))
               (let ,bindings
                 ,expression))
            `(progn ()
                    (warn "The expression ~%~a~% doesn't depend on the nlopt parameters ~%" expression vars)
                    `(lambda (,x ,grad, ,nlopt)
                       (declare (ignore ,grad ,nlopt))
                       ,expression))))))

  (defun %with-nlopt-optimize (vars type functions)
    "Return code for optimizing (first functions)"
    (unless (= 1 (length functions))
      (error ":minimize or :maximize take only one expression ~a" functions))
    (ecase type
      (:minimize `(set-min-objective *nlopt-instance*
                                     ,(%with-nlopt-lambda vars (first functions))))
      (:maximize `(set-max-objective *nlopt-instance*
                                     ,(%with-nlopt-lambda vars (first functions))))))


  (defun %with-nlopt-inequality-constraint (vars exp)
    "constaints of type (< (- (* 2 x) 5) (* 2 y) z 10)
input `exp' should not contain inequality sign"
    (assert (> (length exp) 1) (exp))
    (loop
      with code = nil
      for l in exp
      for g in (rest exp) do
        ;; handle expressions of form l < g
        (cond
          ;; x < const
          ((and (member l vars) (no-visible-vars? vars g))
           (push `(setf (upper-bound *nlopt-instance* ,(position l vars)) ,g) code))
          ;; const < x
          ((and (no-visible-vars? vars l) (member g vars))
           (push `(setf (lower-bound *nlopt-instance* ,(position g vars)) ,l) code))
          ;; const < const
          ((and (no-visible-vars? vars l) (no-visible-vars? vars g))
           (error "~a < ~a inequality doesn't involve any of the declared parameters ~a" l g vars))
          ;; x < f(vars), f(vars) < g(vars), f(vars) < x i.e. l - g < 0
          (t
           (push `(add-inequality-constraint *nlopt-instance*
                                             ,(%with-nlopt-lambda vars (list '- l g)))
                 code)))
      finally (return (reverse code))))

  (defun %with-nlopt-constraints (vars exp)
    (cond ((member (first exp) '(> < <= >=))
           ;; inequality constraint (<)
           (if (or (eql (first exp) '>) (eql (first exp) '>=))
               (setf exp (reverse (rest exp)))
               (setf exp (rest exp)))
           (let ((code (%with-nlopt-inequality-constraint vars exp)))
             (if (= 1 (length code)) ;; single expression
                 (first code)
                 `(progn ,@code))))
          (t ;; equality constraint
           `(add-equality-constraint *nlopt-instance* ,(%with-nlopt-lambda vars exp)))))

  (defun %with-nlopt-satisfy (vars body)
    `(progn
       ,@(loop for exp in body
               collect (%with-nlopt-constraints vars exp))))

  (defun %with-nlopt-walker (vars body)
    "Walk the body and replace :minimize, :maximize and :satisfy with proper code"
    (cond ((null body) nil)
          ((not (listp body)) body)

          ((eql (first body) 'quote) body)
          ((keywordp (first body))
           (ecase (first body)
             (:minimize (%with-nlopt-optimize vars :minimize (rest body)))
             (:maximize (%with-nlopt-optimize vars :maximize (rest body)))
             (:satisfy (%with-nlopt-satisfy vars (rest body)))))
          (t (mapcar #'(lambda (exp) (%with-nlopt-walker vars exp)) body))))

  (defmacro nloptimize ((vars &key (algorithm :nlopt_ln_cobyla) (initial '(1 1 1))) &body body)
    "Quick and easy way to solve an optimization problem
`vars' is list of symbols for variables/parameter names
`algorithm' is the algorithm used for solving. It must be a non-gradient algorithm :nlopt_ln_* or :nlopt_gn_*
`initial' is the initial values used for parameters
`body' can contain normal lisp code however the `body' is parsed for following covenience forms
  (:minimize expr) (:maximize expr) ;; sets the objective function
  (:satisfy expr1 expr2 ...)        ;; adds constraints
     each constraint can be of the form
         (+|-  ...)       ;; an equality constraint expr = 0
         (<|<=|>|>=  ...) ;; an inequality constraint or bounds on the parameter
inside the body `*nlopt-instance*' can be used to refer to current nlopt instance"

    `(let ((*nlopt-instance* (create ,algorithm ,(length vars))))
       ,@(%with-nlopt-walker vars body)
       (optimize-nlopt *nlopt-instance* (doubles* ,initial)))))
