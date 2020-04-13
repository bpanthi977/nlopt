# nlopt (Common Lisp NLopt Bindings)
### _Bibek Panthi <bpanthi977@gmail.com>_

This is a Common Lisp interface to NLopt. NLopt is a free/open-source library for nonlinear optimization, providing a common interface for a number of different free optimization routines 
available online as well as original implementations of various other algorithms. See https://nlopt.readthedocs.io/en/latest/ for more information on the available algorithms.

## Use 

`common-lisp 
(in-package :nlopt)
(defun solve()
  (let ((nlopt (create :nlopt_ld_mma 2)))
	(set-lower-bounds nlopt (darray -11d0 0.001d0))
	(set-min-objective nlopt (lambda (x grad nlopt)
							  ;; compute f(x) and grad f(x) at x
							  (set-darray grad . .)
							  (f x)))
	(add-inequality-constraint nlopt
							   (lambda (x grad nlopt)
						       ;; compute h1(x) and grad h1(x)
							   (set-darray grad . .)
							   (h1 x))
							   1d-2)
	(add-inequality-constraint nlopt
	                           (lambda (x grad nlopt)
							   ... compute h2(x) and grad h2(x) ..)
							   1d-2)
	(set-xtol-rel nlopt 1d-4)
	(optimize-nlp nlopt (darray 1.234d0 5.67d0))))
`
	

First you create a nlopt object representing the optimization problem by specifying the algorithm to use and the dimension (number of variables) of the problem. 
`common-lisp 
(create algorithm dimension)
` 
Then you add objective function, constraints, bounds, stopping criteria, etc to the problem. E.g. 
`common-lisp 
(set-min-objective nlopt (lambda (x grad nlopt) ...))
`
Here an objective to minimize the function (lambda) is set. 
This callback function (lambda) accepts the value of variables (x) in an foreign-array of double-float. Elements of x can be accessed by using `(dref x index)` function. 
Also if the algorithm selected requires the gradient of the function to be computed then grad is set to a pointer to the foreign-array where calculated values have to set. 
You can use `(setf (dref grad 0) value0 (dref grad 1) value1 ...)` or `(set-darray grad value0 value1)`. If the algorithm doesn't require gradient to be calculated then  `grad` is NIL. 
The current `nlopt` object is also passed to the callback. 

### Conventions 
You should read the documentation of NLopt library at https://nlopt.readthedocs.io/en/latest/NLopt_Reference/ . This Common Lisp binding to the NLopt library tries to follow the 
names of the original functions albiet with some lispier modifications. Let's see few example: 

* `c 
nlopt_create(nlopt_algorithm algorithm, unsigned n);
` is available as 
`common-lisp 
(create algorithm n)
` The nlopt prefix is removed because all functions are in nlopt package. So, you may access them from any other package as `(nlopt:create algorithm n)` 


* `c 
nlopt_set_min_objective(nlopt_opt opt, nlopt_func f, void* f_data);
`  is available as 
`common-lisp 
(set-min-objective nlopt function)
` The nlopt prefix is removed and all underscores are converted to hypens. Also notice that, the function doesn't take a `f_data` struct to pass along the callbacks. 
	Because in Common Lisp we can use dynamic binding to set the contex. Also, instead of `f_data`, the nlopt instance is passed along in the callbacks. So, you may create a subclass of nlopt if you need 
	to store some state to use in the callbacks. 
	
* The callback for the objective function 
`c 
double f(unsigned n, const double* x, double* grad, void* f_data);
` should be defined as 
`common-lisp 
(defun f(x grad nlopt)
		...)`
As mentioned above instead of a user defined data `f_data`, the `nlopt` instance is passed. Also note that any parameters indicating dimensions (such as `n`, `m`) as also unnecessary because
they are available through the `nlopt` object as : `n` = `  (dimension nlopt)`. 
* Since `optimize` is a reserved word in Common Lisp, `c 
nlopt_optimize(nlopt_opt opt, double *x, double *opt_f);` is avaibale as 
`commmon-lisp 
(optimize-nlp nlopt initial-x)`. Since functions can return multiples values, the `opt_f` pointer is not required to get the optimum value. `optimize` returns the optimum position `x` and the optimum value `f(x)`.

* All other functions are name similarly. 
* The current lispy interface (`nlopt:` package) is not complete. However all the function bindings of NLopt are avaibale in `nlopt.cffi:` package. 
So, you can use all the features provided by NLopt without much problem. 

## Examples 
Few examples are available in examples.lisp file. It may be helpful to follow along the tutorial https://nlopt.readthedocs.io/en/latest/NLopt_Tutorial/ along with its implementation given in example.lisp file `(example-mma)` 

## Installation 
### Install the NLopt shared library to your system 
#### Linux
* Ubuntu
`bash 
apt install nlopt 
`
* Arch
`bash 
pacman -Suy nlopt
`

#### Windows 
Download the prebuilt binary from 
https://nlopt.readthedocs.io/en/latest/NLopt_on_Windows/
and copy the libnlopt.dll to any folder in your path (e.g. c:/Windows/System32/) or to this project's directory 

### Install this library 
* Download or clone this repo to your local-projects directory then 
* `common-lisp 
  (ql:quickload :nlopt)
  ` it

## License
LGPL-3.0
https://choosealicense.com/licenses/lgpl-3.0/


