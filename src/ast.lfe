(defmodule ast
  (export (make-variable 1)
	  (make-literal-list 2)
	  (make-condition 1)
	  (make-application 1)
	  (make-abstraction 1)))

(defun make-variable (identifier)
  (tuple 'variable identifier))

(defun make-condition
  (((tuple predicate (tuple then else))) (tuple 'condition predicate then else)))

(defun make-application
  (((tuple function arguments)) (tuple 'application function arguments)))

(defun make-argument (arg)
  (tuple 'argument arg))

(defun make-rest (str)
  (tuple 'rest str))

(defun make-abstraction
  (((tuple (tuple parameters "") body)) (tuple 'abstraction (tuple 'notVariadic (lists:map #'make-argument/1 parameters)) body))
  (((tuple (tuple parameters variadic) body)) (tuple 'abstraction (lists:append (lists:map #'make-argument/1 parameters)) (list (make-rest variadic))) body))

(defun make-literal-list (type transformation)
  (lambda (arg)
    (flet ((wrap-literal (value)
			 (tuple 'literal (tuple type (funcall transformation value)))))
      (funcall #'wrap-literal/1 arg))))