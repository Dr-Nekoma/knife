(defmodule knife
  (export (parse 1))
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
		(empty 0)
		(invalid-lambda 1)
		(any-boolean 0)
		(parser/map 2)
		(parser/bind 2)
		(whitespaces* 0)
		(whitespaces+ 0))
	  (from ast (make-variable 1)
		(make-literal-list 2)
		(make-condition 1)
		(make-application 1)
		(make-abstraction 1))
	  (from utils (string-to-integer 1)
		(string-to-boolean 1)
		(id 1))))

(defun parse (file-content)
  (funcall (get-parser (expression)) (build-input file-content)))

(defun expression ()
  (list-alt (list (condition) (application) (abstraction) (literal) (variable))))

(defun variable ()
  (parser/map #'ast:make-variable/1 (parser/bind #'basic:invalid-lambda/1 (identifier))))

(defun literal ()
  (list-alt (list (any-number) (any-boolean))))

(defmacro parser-header (parser)
  `(build-parser
    `,(lambda (input)
	(funcall (get-parser ,parser) input))))

(defun application ()
  (parser/map
   #'ast:make-application/1
   (parser-header
    (justRight
     (app (char "[") (whitespaces*))
     (justLeft
      (app (expression) (many* (justRight (whitespaces+) (expression))))
      (app (whitespaces*) (char "]")))))))

(defun parameters ()
  (parser-header
   (justRight
    (app (char "[") (whitespaces*))
    (justLeft
     (app (many* (justLeft (identifier) (whitespaces*))) (optional (variadic)))
     (app (whitespaces*) (char "]"))))))

(defun abstraction ()
  (parser/map
   #'ast:make-abstraction/1
   (parser-header
    (justRight
     (app (char "[") (whitespaces*))
     (justLeft
      (app
       (justRight (justLeft (prefix "lambda") (whitespaces*)) (parameters))
       (many+ (justRight (whitespaces+) (expression))))
      (app (whitespaces*) (char "]")))))))

(defun condition ()
  (parser/map
   #'ast:make-condition/1
   (parser-header
    (justRight
     (app (char "[") (whitespaces*))
     (justLeft
      (app
       (justRight (app (prefix "if") (whitespaces+)) (expression))
       (app (justRight (whitespaces+) (expression)) (justRight (whitespaces+) (expression))))
      (app (whitespaces*) (char "]")))))))