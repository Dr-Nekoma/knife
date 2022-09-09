(defmodule ast
  (export (make-variable 1)
	  (make-literal-list 2)))

(defun make-variable
  (((tuple identifier arguments)) (tuple 'application (tuple 'variable identifier) arguments))
  ((else) (io:fwrite "Couldn't make variable with ~p~n" (list else))))

(defun make-literal-list (type transformation)
  (lambda (arg)
    (flet ((wrap-literal (value)
			 (tuple 'literal (tuple type (funcall transformation value)))))
      (funcall #'wrap-literal/1 arg))))