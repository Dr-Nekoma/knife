(defmodule knife
  (export (main 1))
  (import (from basic (many 1)
		(parse-run 1)
		(char 1)
		(number 1)
		(make-parser 2)
		(justLeft 2)
		(justRight 2)
		(any-char 0))))

(defun main (argv)
  '())

(set filepath "/home/lemos/Programming/DrNekoma/knife/examples/arithmetic.sw")

(defun read-file ()
  (case (file:open filepath (list 'read))
    ((tuple 'error reason) (io:fwrite "Didn't find file"))
    ((tuple 'ok descriptor) (file:read descriptor 1000000))))

(defun s-expression ()
  (make-parser
   run
   (lambda (input)
     (funcall
      (parser-run
       (justRight
	(char "[")
	(justLeft
	 (many (char "a"))
	 (char "]")))) input))))

