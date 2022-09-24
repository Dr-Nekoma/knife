(defmodule utils
  (export (string-to-integer 1)
	  (string-to-boolean 1)
	  (get-second 1)))

(defun string-to-integer (digits)
  (let ((integer (string:to_integer digits)))
    (tuple 'literal (tuple 'integer (tref integer 1)))))

(defun get-second
  (((tuple f s)) s))
  
(defun string-to-boolean
  (("T") 'true)
  (("F") 'false))

