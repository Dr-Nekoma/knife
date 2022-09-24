(defmodule utils
  (export (string-to-integer 1)
	  (string-to-boolean 1)
	  (get-second 1)))

(defun string-to-integer (str)
  (tref (string:to_integer str) 1)) 

(defun get-second
  (((tuple f s)) s))
  
(defun string-to-boolean
  (("T") 'true)
  (("F") 'false))

