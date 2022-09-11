(defmodule utils
  (export (string-to-integer 1)
	  (string-to-boolean 1)
	  (id 1)))

(defun string-to-integer (str)
  (tref (string:to_integer str) 1))

(defun string-to-boolean
  (("T") 'true)
  (("F") 'false))

(defun id (x) x)

