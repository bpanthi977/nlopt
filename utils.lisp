(in-package :nlopt)

(deftype darray ()
  "Array of double floats"
  '(array double-float *))

(defgeneric darray (first &rest rest))

(defmethod darray ((first double-float) &rest rest)
  "Create an array of double-float"
  (make-array (1+ (length rest)) :element-type 'double-float :initial-contents (cons first rest)))

(declaim (inline dref))
(defun dref (ptr index)
  "Index a foreign array of double float"
  (cffi:mem-aref ptr :double index))

(defun (setf dref) (val ptr index)
  (setf (cffi:mem-aref ptr :double index) val))


(defun setf-darray (ptr &rest data)
  "Set values of a foreign array of doubles"
  (loop for d in data
	 for i from 0 do
	   (setf (cffi:mem-aref ptr :double i) d)))


(defun dreffing@/0 (var expr)
  (print expr)
  (cond ((atom expr)
		 (if (and (symbolp expr)
				  (char-equal #\@ (aref (symbol-name expr) 0)))
			 `(dref ,(parse-integer (symbol-name expr) :start 1) ,var)
			 expr))
		((listp expr)
		 (cons (first expr) (mapcar (lambda (e) (dreffing@/0 var e)) (rest expr))))))

(defmacro dreffing@ (x &body body)
  "replace occurance of @n with (dref x n) in body"
  `(progn ,@(mapcar (lambda (e) (dreffing@/0 x e))
					body)))


(defun foreign-darray-to-lisp (ptr dimension)
  (let ((arr (make-array dimension :element-type 'double-float)))
	(loop for i from 0 below dimension do
		 (setf (aref arr i) (cffi:mem-aref ptr :double i)))
	arr))
