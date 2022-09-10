(defmodule knife
  (export (main 0)
	  (debug 2))
  (import (from basic (many+ 1)
		(many* 1)
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
		(alt 2)
		(prefix 1)
		(optional 1)
		(variadic 0)
		(predicate-whitespace 1)
		(any-char 0)
		(any-boolean 0)
		(parser/map 2)
		(whitespaces* 0)
		(whitespaces+ 0))
	  (from ast (make-variable 1)
		(make-literal-list 2))
	  (from utils (string-to-integer 1)
		(string-to-boolean 1)
		(id 1))))

(defun main ()
  (funcall (get-parser (expression)) (build-input "[if T T T]")))

(defun debug (parser str)
  (funcall (get-parser parser) (build-input str)))

;; (set filepath "/home/lemos/Programming/DrNekoma/knife/examples/arithmetic.sw")

;; (defun read-file ()
;;   (case (file:open filepath (list 'read))
;;     ((tuple 'error reason) (io:fwrite "Didn't find file"))
;;     ((tuple 'ok descriptor) (file:read descriptor 1000000))))

(defun expression ()
  (parser-header
   (list-alt (list (literal)
;;		   (application)
;;		   (abstraction)
		   (condition)
		   (variable)))))

(defun application ()
  (parser-header
   (justRight
    (app (char "[") (whitespaces*))
    (justLeft
     (app (expression) (many* (justRight (whitespaces+) (expression))))
     (app (whitespaces*) (char "]"))))))

(defun variable ()
  (parser-header (parser/map #'ast:make-variable/1 (identifier))))

(defun literal ()
  (parser-header
   (list-alt (list (any-number) (any-boolean)))))

(defun parser-header (parser)
  (build-parser
   (lambda (input)
     (funcall
      (get-parser parser) input))))

(defun parameters ()
  (parser-header
   (justRight
    (app (char "[") (whitespaces*))
    (justLeft
     (app (many* (justLeft (identifier) (whitespaces*))) (optional (variadic)))
     (app (whitespaces*) (char "]"))))))

(defun abstraction ()
 (parser-header
   (justRight
    (app (char "[") (whitespaces*))
    (justLeft
     (app
      (app (justLeft (prefix "lambda") (whitespaces*)) (parameters))
      (many+ (justRight (whitespaces+) (expression))))
     (app (whitespaces*) (char "]"))))))

(defun condition ()
  (parser-header
   (justRight
    (app (char "[") (whitespaces*))
    (justLeft
     (app
      (justRight (app (prefix "if") (whitespaces+)) #'expression/0)
      (app (justRight (whitespaces+) #'expression/0) (justRight (whitespaces+) #'expression/0)))
     (app (whitespaces*) (char "]"))))))
