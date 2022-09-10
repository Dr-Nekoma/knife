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
		(any-char 0)
		(any-boolean 0)
		(parser/map 2))
	  (from ast (make-variable 1)
		(make-literal-list 2))
	  (from utils (string-to-integer 1)
		(string-to-boolean 1)
		(id 1))))

(defun main ()
  (funcall (get-parser (s-expression)) (build-input "[&& t t f]")))

(defun debug (parser str)
  (funcall (get-parser parser) (build-input str)))

;; (set filepath "/home/lemos/Programming/DrNekoma/knife/examples/arithmetic.sw")

;; (defun read-file ()
;;   (case (file:open filepath (list 'read))
;;     ((tuple 'error reason) (io:fwrite "Didn't find file"))
;;     ((tuple 'ok descriptor) (file:read descriptor 1000000))))

;; (defun application ()
;;   (parser-header
;;    (justRight
;; 	(app (char "[") (whitespaces*))
;; 	(justLeft
;; 	 (app (expression) (many* (justRight (whitespaces) (expression)))
;; 	 (app (whitespaces*) (char "]")))))))

(defun s-expression ()
  (parser-header
   (justRight
	(char "[")
	(justLeft
	 (boolean-expression)
	 (char "]")))))

(defun parser-header (parser)
  (build-parser
     (lambda (input)
       (funcall
        (get-parser parser) input))))

(defun arithmetic-expression ()
  (variable-expression (identifier) (any-number) (funcall #'ast:make-literal-list/2 'integer #'utils:string-to-integer/1)))

(defun boolean-expression ()
  (variable-expression (identifier) (any-boolean) (funcall #'ast:make-literal-list/2 'boolean #'utils:string-to-boolean/1)))

(defun literal ())

(defun variable-expression (body-parser argument-parser argument-function)
  (parser/map
   #'ast:make-variable/1
   (parser-header
    (app
     (justLeft
      body-parser
      (space))
     (many (justLeft
	    (parser/map argument-function argument-parser)
	    (space)))))))

;; [+ 1 1 2 ]
;; ---
;; {application, {variable, '+'}, [{literal, {integer, 1}}, {literal, {integer, 1}}, {literal, {integer, 2}}]}