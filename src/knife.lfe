(defmodule knife
  (export (main 0)
	  (debug 2))
  (import (from basic (many 1)
		(char 1)
		(any-number 0)
		(justLeft 2)
		(justRight 2)
		(build-parser 1)
		(build-input 1)
		(get-parser 1)
		(list-alt 1)
		(space 0)
		(app 2)
		(identifier 0)
		(while 1)
		(predicate-whitespace 1)
		(any-char 0))))

(defun main ()
  (funcall (get-parser (s-expression)) (build-input "[+ 1 1 2 ]")))

(defun debug (parser str)
  (funcall (get-parser parser) (build-input str)))

;; (set filepath "/home/lemos/Programming/DrNekoma/knife/examples/arithmetic.sw")

;; (defun read-file ()
;;   (case (file:open filepath (list 'read))
;;     ((tuple 'error reason) (io:fwrite "Didn't find file"))
;;     ((tuple 'ok descriptor) (file:read descriptor 1000000))))

(defun s-expression ()
  (build-parser
   (lambda (input)
     (funcall
      (get-parser
       (justRight
	(char "[")
	(justLeft
	 (variable-expression)
	 (char "]")))) input))))

(defun parser-header (parser)
  (build-parser
     (lambda (input)
       (funcall
         (get-parser parser) input))))

(defun variable-expression ()
  (parser-header
   (app
    (justLeft
     (identifier)
     (space))
    (many (justLeft
	   (any-number)
	   (space))))))

;; [+ 1 1 2 ]
;; ---
;; {application, {variable, '+'}, [{literal, {integer, 1}}, {literal, {integer, 1}}, {literal, {integer, 2}}]}
  
