(in-package :nlopt)

(deftype doubles ()
  "Array of double floats"
  '(array double-float *))

(defun doubles (list)
  (make-array (length list) :element-type 'double-float
							:initial-contents list))

(defun doubles* (list)
  (make-array (length list) :element-type 'double-float
							:initial-contents (loop for x in list
													collect (coerce x 'double-float))))

(defun doubles-array (size initial-element)
  (make-array size :element-type 'double-float
				   :initial-element initial-element))

(defun doubles-array* (size initial-element)
  (make-array size :element-type 'double-float
				   :initial-element (coerce initial-element 'double-float)))

(declaim (inline dref))
(defun dref (ptr index)
  "Index a foreign array of double float"
  (cffi:mem-aref ptr :double index))

(defun (setf dref) (val ptr index)
  (setf (cffi:mem-aref ptr :double index) val))


(defun setf-doubles (ptr &rest data)
  "Set values of a foreign array of doubles"
  (loop for d double-float in data
		for i integer from 0 do
	   (setf (cffi:mem-aref ptr :double i) d)))


(defun %dreffing@ (var expr)
  (print expr)
  (cond ((atom expr)
		 (if (and (symbolp expr)
				  (char-equal #\@ (aref (symbol-name expr) 0)))
			 `(dref ,(parse-integer (symbol-name expr) :start 1) ,var)
			 expr))
		((listp expr)
		 (cons (first expr) (mapcar (lambda (e) (%dreffing@ var e)) (rest expr))))))

(defmacro dreffing@ (x &body body)
  "replace occurance of @n with (dref x n) in body"
  `(progn ,@(mapcar (lambda (e) (%dreffing@ x e))
					body)))

;; (defun foreign-darray-to-lisp (ptr dimension)
;;   (let ((arr (make-array dimension :element-type 'double-float)))
;; 	(loop for i from 0 below dimension do
;; 		 (setf (aref arr i) (cffi:mem-aref ptr :double i)))
;; 	arr))

(defun foreign-darray-to-lisp (ptr) 
  (cffi:foreign-array-to-lisp ptr 'double-float))
