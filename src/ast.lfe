(defmodule ast
  (export (make-variable 1)
	  (make-literal-list 1)))

(defun make-variable
  (((tuple identifier arguments)) (tuple 'application (tuple 'variable identifier) arguments))
  ((else) (io:fwrite "Couldn't make variable with ~p~n" (list else))))

(defun make-literal-list (type)
  (lambda (args)
    (flet ((wrap-literal (value)
			 (tuple 'literal (tuple type value))))
      (lists:map #'wrap-literal/1 args))))