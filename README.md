# nlopt (Common Lisp NLopt Bindings)
### _Bibek Panthi <bpanthi977@gmail.com>_

This is a Common Lisp interface to NLopt. NLopt is a free/open-source library for nonlinear optimization, providing a common interface for a number of different free optimization routines 
available online as well as original implementations of various other algorithms. See https://nlopt.readthedocs.io/en/latest/ for more information on the available algorithms.

## Use 

```common-lisp 
(in-package :nlopt)
(defun solve()
  (let ((nlopt (create :nlopt_ld_mma 2)))
	(set-lower-bounds nlopt (darray -11d0 0.001d0))
	(set-min-objective nlopt objective-function)
	(add-inequality-constraint nlopt constraint1)
	(add-inequality-constraint nlopt constraint2)
	(set-xtol-rel nlopt 1d-4)
	(optimize-nlp nlopt (darray 1.234d0 5.67d0))))
```
First you create a nlopt object representing the optimization problem by specifying the algorithm to use and the dimension (number of variables) of the problem. 

```common-lisp 
(create algorithm dimension)
```
Then you add objective function, constraints, bounds, stopping criteria, etc to the problem. E.g. 
```common-lisp 
(defun objective-function (x grad nlopt)
  (setf-doubles grad grad_0 grad_1 grad_2 ...)
  objective-function-value-at-x)

(set-min-objective nlopt objective-function))
```
Here an objective to minimize the function (objective-function) is set. 
This callback function (or lambdas can also be used) accepts the value of variables (x) in an foreign-array of double-float. Elements of x can be accessed by using `(dref x index)` function. 
Also if the algorithm selected requires the gradient of the function to be computed then grad is set to a pointer to the foreign-array where calculated values have to set. 
You can use `(setf (dref grad 0) value0 (dref grad 1) value1 ...)` or `(set-doubles grad value0 value1 ...)`. If the algorithm doesn't require gradient to be calculated then  `grad` is NIL. 
The current `nlopt` object is also passed to the callback. 

### Conventions 
You should read the documentation of NLopt library at https://nlopt.readthedocs.io/en/latest/NLopt_Reference/ . This Common Lisp binding to the NLopt library tries to follow the 
names of the original functions albiet with some lispier modifications. Let's see few example: 

```c 
nlopt_create(nlopt_algorithm algorithm, unsigned n);
```
is available as 
```common-lisp 
(create algorithm n)
``` 
The nlopt prefix is removed because all functions are in nlopt package. So, you may access them from any other package as `(nlopt:create algorithm n)` 

```c 
nlopt_set_min_objective(nlopt_opt opt, nlopt_func f, void* f_data);
``` 
is available as 
```common-lisp 
(set-min-objective nlopt function)
```
The nlopt prefix is removed and all underscores are converted to hypens. Also notice that, the function doesn't take a `f_data` struct to pass along the callbacks. 
	Because in Common Lisp we can use dynamic binding to set the contex. Also, instead of `f_data`, the nlopt instance is passed along in the callbacks. So, you may create a subclass of nlopt if you need 
	to store some state to use in the callbacks. 
	
The callback for the objective function 
```c 
double f(unsigned n, const double* x, double* grad, void* f_data);
```
should be defined as 
```common-lisp 
(defun f(x grad nlopt) ...)	
```
As mentioned above instead of a user defined data `f_data`, the `nlopt` instance is passed. Also note that  parameter indicating dimension (`n`) as also unnecessary because
it is available through the `nlopt` object as : `n` = `  (dimension nlopt)`. 


Signature for callbacks to other functions are documented in the docstring of respective functions (`set-min-objective`, `set-inequality-constraints`, ...)

Since `optimize` is a reserved word in Common Lisp, `c 
nlopt_optimize(nlopt_opt opt, double *x, double *opt_f);` is avaibale as 
`commmon-lisp 
(optimize-nlp nlopt initial-x)`. Since functions can return multiples values, the `opt_f` pointer is not required to get the optimum value. `optimize` returns the optimum position `x` and the optimum value `f(x)`.


Functions that have `set-foo` and `get-foo` counterparts are defined as `(setf (foo ...) value)` and `(foo ...)`

All other functions are name similarly. 
#### The NLopt Object
| NLopt                                            | Common Lisp Binding        |
|--------------------------------------------------|----------------------------|
| nlopt_destroy(nlopt_opt opt)                     |                            |
| nlopt_copy(const nlopt_opt opt)                  | (copy nlopt)               |
| nlopt_get_algorithm(const nlopt_opt opt);        | (algorithm nlopt)          |
| nlopt_get_dimension(const nlopt_opt opt);        | (dimensions nlopt)         |
| nlopt_algorithm_name(nlopt_algorithm algorithm); | (algorithm-name algorithm) |

#### Objective Function
| NLopt                                                                                          | Common Lisp Binding                            |
|------------------------------------------------------------------------------------------------|------------------------------------------------|
| nlopt_set_min_objective(nlopt_opt opt, nlopt_func f, void* f_data);                            | (set-min-objective nlopt function)             |
| nlopt_set_max_objective(nlopt_opt opt, nlopt_func f, void* f_data);                            | (set-max-objective nlopt function)             |
| nlopt_set_precond_min_objective(nlopt_opt opt, nlopt_func f, nlopt_precond pre, void *f_data); | (set-precond-min-objective nlopt function pre) |
| nlopt_set_precond_min_objective(nlopt_opt opt, nlopt_func f, nlopt_precond pre, void *f_data); | (set-precond-max-objective nlopt function pre) |

#### Constraints and Bounds 
| NLopt                                                                                                        | Common Lisp Binding                               |
|--------------------------------------------------------------------------------------------------------------|---------------------------------------------------|
| nlopt_add_inequality_constraint(nlopt_opt opt, nlopt_func fc, void* fc_data, double tol);                    | (add-inequality-constraint nlopt function tol)    |
| nlopt_add_equality_constraint(nlopt_opt opt, nlopt_func h, void* h_data, double tol);                        | (add-equality-constraint nlopt function tol)      |
| nlopt_remove_inequality_constraints(nlopt_opt opt);                                                          | (remove-inequality-constraint nlopt)              |
| nlopt_remove_equality_constraints(nlopt_opt opt);                                                            | (remove-equality-constraint nlopt)                |
| nlopt_add_inequality_mconstraint(nlopt_opt opt, unsigned m, nlopt_mfunc c, void* c_data, const double *tol); | (add-inequality-mconstraint nlopt m function tol) |
| nlopt_add_equality_mconstraint(nlopt_opt opt, unsigned m, nlopt_mfunc c, void* c_data, const double *tol);   | (add-equality-mconstraint nlopt m function tol)   |
| nlopt_set_lower_bounds(nlopt_opt opt, const double* lb);                                                     | (setf (lower-bounds nlopt) bounds)                |
| nlopt_set_upper_bounds(nlopt_opt opt, const double* ub);                                                     | (setf (upper-bounds nlopt) bounds)                |
| nlopt_set_lower_bounds1(nlopt_opt opt, double lb);                                                           | (setf (lower-bounds nlopt) bound)                 |
| nlopt_set_upper_bounds1(nlopt_opt opt, double ub);                                                           | (setf (upper-bounds nlopt) bound)                 |
| nlopt_get_lower_bounds(const nlopt_opt opt, double* lb);                                                     | (lower-bounds nlopt)                              |
| nlopt_get_upper_bounds(const nlopt_opt opt, double* ub);                                                     | (upper-bounds nlopt)                              |
| nlopt_set_lower_bound(nlopt_opt opt, int i, double lb);                                                      | (setf (lower-bound nlopt i) bound)                |
| nlopt_set_upper_bound(nlopt_opt opt, int i, double ub);                                                      | (setf (upper-bound nlopt i) bound)                |
|                                                                                                              | (lower-bound nlopt i)                             |
|                                                                                                              | (upper-bound nlopt i)                             |

#### Stopping Criteria 
| NLopt                                                 | Common Lisp Binding              |
|-------------------------------------------------------|----------------------------------|
| nlopt_set_stopval(nlopt_opt opt, double stopval);     | (setf (stopval nlopt) stopval)   |
| nlopt_get_stopval(const nlopt_opt opt);               | (stopval nlopt)                  |
| nlopt_set_ftol_rel(nlopt_opt opt, double tol);        | (setf (ftol-rel nlopt) tol)      |
| nlopt_get_ftol_rel(const nlopt_opt opt);              | (ftol-rel nlopt)                 |
| nlopt_set_ftol_abs(nlopt_opt opt, double tol);        | (setf (ftol_abs nlopt) tol)      |
| nlopt_get_ftol_abs(const nlopt_opt opt);              | (ftol-abs nlopt)                 |
| nlopt_set_xtol_rel(nlopt_opt opt, double tol);        | (setf (xtol-rel nlopt) tol)      |
| nlopt_get_xtol_rel(const nlopt_opt opt);              | (xtol-rel nlopt)                 |
| nlopt_set_x_weights(nlopt_opt opt, const double *w);  | (setf (x-weights nlopt) weights) |
| nlopt_set_x_weights1(nlopt_opt opt, const double w);  | (setf (x-weights nlopt) weight)  |
| nlopt_get_x_weights(const nlopt_opt opt, double *w);  | (x-weights nlopt)                |
| nlopt_set_xtol_abs(nlopt_opt opt, const double *tol); | (setf (xtol-abs nlopt) tol)      |
| nlopt_set_xtol_abs1(nlopt_opt opt, double tol);       | (setf (xtol-abs nlopt) tol)      |
| nlopt_get_xtol_abs(const nlopt_opt opt, double *tol); | (xtol-abs nlopt)                 |
| nlopt_set_maxeval(nlopt_opt opt, int maxeval);        | (setf (maxval nlopt) maxeval)    |
| nlopt_get_maxeval(nlopt_opt opt);                     | (maxeval nlopt)                  |
| nlopt_set_maxtime(nlopt_opt opt, double maxtime);     | (setf (maxtime nlopt) maxtime)   |
| nlopt_get_maxtime(nlopt_opt opt);                     | (maxtime nlopt)                  |
| nlopt_get_numevals(nlopt_opt opt);                    | (numevals nlopt)                 |

#### Forced Termination 
| NLopt                                        | Common Lisp Binding                 |
|----------------------------------------------|-------------------------------------|
| nlopt_force_stop(nlopt_opt opt);             | (force-stop nlopt)                  |
| nlopt_set_force_stop(nlopt_opt opt, int val) | (setf (force-stop-value nlopt) val) |
| nlopt_get_force_stop(nlopt_opt opt)          | (force-stop-value nlopt)            |

#### Optimization 
| NLopt                                                                | Common Lisp Binding                     |
|----------------------------------------------------------------------|-----------------------------------------|
| nlopt_optimize(nlopt_opt opt, double *x, double *opt_f);             | (optimize-nlopt nlopt x)                |
| nlopt_set_local_optimizer(nlopt_opt opt, const nlopt_opt local_opt); | (set-local-optimizer nlopt local-nlopt) |

#### Others 
| NLopt                                                                     | Common Lisp Binding             |
|---------------------------------------------------------------------------|---------------------------------|
| nlopt_set_initial_step(nlopt_opt opt, const double* dx);                  | (setf (initial-step nlopt) dx)  |
| nlopt_set_initial_step1(nlopt_opt opt, const double dx);                  | (setf (initial-step nlopt) dx)  |
| nlopt_get_initial_step(const nlopt_opt opt, const double *x, double *dx); | (initila-step nlopt)            |
| nlopt_set_population(nlopt_opt opt, unsigned pop);                        | (set-population nlopt size)     |
| nlopt_srand(unsigned long seed);                                          | (srand seed)                    |
| nlopt_srand_time(void);                                                   | (srand-time)                    |
| nlopt_set_vector_storage(nlopt_opt opt, unsigned M);                      | (setf (vector-storage nlopt) M) |
| nlopt_get_vector_storage(const nlopt_opt opt);                            | (vector-storage nlopt)          |
| nlopt_version(int *major, int *minor, int *bugfix);                       | (version)                       |


## Examples 
Few examples are available in examples.lisp file. It may be helpful to follow along the tutorial https://nlopt.readthedocs.io/en/latest/NLopt_Tutorial/ along with its implementation given in example.lisp file `(example-mma)` 

## Installation 
### Install the NLopt shared library to your system 
#### Linux
* Ubuntu
```bash 
apt install nlopt 
```
* Arch
```bash 
pacman -Suy nlopt
```

#### Windows 
Download the prebuilt binary from 
https://nlopt.readthedocs.io/en/latest/NLopt_on_Windows/
and copy the libnlopt.dll to any folder in your PATH (e.g. c:/Windows/System32/) or to this project's directory 

### Install this library 
Download or clone this repo to your local-projects directory then 
```common-lisp
(ql:quickload :nlopt)
```
it 

## License
LGPL-3.0
https://choosealicense.com/licenses/lgpl-3.0/


