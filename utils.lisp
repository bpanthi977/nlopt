(in-package :nlopt)

(defmacro ensure-success (&body body)
  `(assert (eql :nlopt_success (progn ,@body))))

(deftype doubles ()
  "Array of double floats"
  '(array double-float *))

(defun doubles (list)
  "Create an array of double floats from given `list' of double floats"
  (make-array (length list) :element-type 'double-float
							:initial-contents list))

(defun doubles* (list)
  "Create an array of double floats from given `list' (coerces numbers to double-float)"
  (make-array (length list) :element-type 'double-float
							:initial-contents (loop for x in list
													collect (coerce x 'double-float))))

(defun doubles-array (size initial-element)
  "Create oan array of double floats of `size' with `initial-element'"
  (make-array size :element-type 'double-float
				   :initial-element initial-element))

(defun doubles-array* (size initial-element)
  "Create oan array of double floats of `size' with `initial-element' (coerces `initial-element' to double-float)"
  (make-array size :element-type 'double-float
				   :initial-element (coerce initial-element 'double-float)))

(declaim (inline dref))
(defun dref (ptr index)
  "Return element of foreign double float array (`ptr') at `index' position"
  (cffi:mem-aref ptr :double index))

(defun (setf dref) (val ptr index)
  "Set element of foreign double float array `ptr' at `index' to value `val'"
  (setf (cffi:mem-aref ptr :double index) val))

(defun setf-doubles2 (ptr data)
  "Set elements of a foreign array of doubles"
  (loop for d double-float in data
		for i integer from 0 do
	   (setf (cffi:mem-aref ptr :double i) d)))

(defun setf-doubles (ptr &rest data)
  "Set elements of a foreign array of doubles"
  (loop for d double-float in data
		for i integer from 0 do
	   (setf (cffi:mem-aref ptr :double i) d)))

(defun %dreffing@ (var expr)
  (cond ((atom expr)
		 (if (and (symbolp expr)
				  (char-equal #\@ (aref (symbol-name expr) 0)))
			 `(dref ,var ,(parse-integer (symbol-name expr) :start 1))
			 expr))
		((listp expr)
		 (cons (first expr) (mapcar (lambda (e) (%dreffing@ var e)) (rest expr))))))

(defmacro dreffing@ (x &body body)
  "replace occurance of @n with (dref x n) in body
Usefull for avoiding "
  `(progn ,@(mapcar (lambda (e) (%dreffing@ x e))
					body)))

(defun foreign-darray-to-lisp (ptr) 
  (cffi:foreign-array-to-lisp ptr 'double-float))

(defmacro with-vector-ptr-to (vector &body body)
  (assert (symbolp vector))
  `(if ,vector
	   (cffi:with-pointer-to-vector-data (,vector ,vector)
		 ,@body)
	   (let ((,vector (cffi:null-pointer)))
		 ,@body)))

		 
