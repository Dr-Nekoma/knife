(defmodule utils
  (export (string-to-integer 1)))

(defun string-to-integer (str)
  (tref (string:to_integer str) 1))

