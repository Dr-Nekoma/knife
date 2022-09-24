(defmodule ast
  (export (make-variable 1)
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

(defun type-to-atom
  (("Integer") 'integer)
  (("Boolean") 'boolean)
  ((arrow) arrow))

(defun make-argument
  (((tuple arg type)) (tuple (type-to-atom type) arg)))

(defun make-abstraction
  (((tuple (tuple parameters '()) body)) (tuple 'abstraction (tuple 'notVariadic (lists:map #'make-argument/1 parameters)) body))
  (((tuple (tuple parameters variadic) body)) (tuple 'abstraction (tuple 'variadic (lists:append (lists:map #'make-argument/1 parameters) (list (make-argument variadic)))) body)))

(defun make-arrow
  (((tuple first-type second-type))
   (let ((return (type-to-atom second-type)))
     (case first-type
       ((tuple non-variadic-args '()) (tuple 'function 'notVariadic (lists:map #'type-to-atom/1 non-variadic-args) return))
       ((tuple non-variadic-args variadic-arg) (let ((args (lists:append (lists:map #'type-to-atom/1 non-variadic-args) (list variadic-arg))))
						 (tuple 'function 'variadic (lists:map #'type-to-atom/1 args) return)))
       ((tuple '() '()) (tuple 'function 'notVariadic '() return))
       ((tuple '() variadic-arg) (tuple 'function 'variadic (list (type-to-atom variadic-arg)) return))))))