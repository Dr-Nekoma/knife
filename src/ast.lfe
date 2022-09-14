(defmodule ast
  (export (make-variable 1)
	  (make-literal-list 2)
	  (make-condition 1)
	  (make-application 1)
	  (make-abstraction 1)
	  (make-arrow 1)))

(defun make-variable (identifier)
  (tuple 'variable identifier))

(defun make-condition
  (((tuple predicate (tuple then else))) (tuple 'condition predicate then else)))

(defun make-application
  (((tuple function arguments)) (tuple 'application function arguments)))

(defun make-argument
  (((tuple arg "Integer")) (tuple 'integer arg))
  (((tuple arg "Boolean")) (tuple 'boolean arg))
  (((tuple arg arrow)) (tuple arrow arg)))

(defun make-abstraction
  (((tuple (tuple parameters '()) body)) (tuple 'abstraction (tuple 'notVariadic (lists:map #'make-argument/1 parameters)) body))
  (((tuple (tuple parameters variadic) body)) (tuple 'abstraction (tuple 'variadic (lists:append (lists:map #'make-argument/1 parameters) (list (make-argument variadic))) body))))

(defun make-arrow
  (((tuple first-type second-type))
   (case first-type
     ((tuple 'variadic real-first-type) (tuple 'function 'variadic (list real-first-type) second-type))
     (_ (tuple 'function 'notVariadic (list first-type) second-type)))))

(defun make-literal-list (type transformation)
  (lambda (arg)
    (flet ((wrap-literal (value)
			 (tuple 'literal (tuple type (funcall transformation value)))))
      (funcall #'wrap-literal/1 arg))))