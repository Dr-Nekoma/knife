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
		(make-abstraction 1)
		(make-arrow 1))
	  (from utils (string-to-integer 1)
		(string-to-boolean 1)
		(id 1)
		(add-variadic-tag 1))))

(defun parse (file-content)
  (case (funcall (get-parser (expression))
		 (build-input file-content))
    ((tuple input status result) (tuple status result))))

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
     (app
      (many*
       (justLeft
	(app
	 (justRight
	  (app (whitespaces*) (char "("))
	  (justRight (whitespaces*) (identifier)))
	 (justLeft
	  (justRight (whitespaces+) (type))
	  (app (whitespaces*) (char ")"))))
	(whitespaces*)))
       (optional (variadic)))
     (app (whitespaces*) (char "]"))))))

(defun variadic ()
  (justRight
   (char "&")
   (justLeft
    (app
     (justRight
      (char "(")
      (identifier))
     (justRight
       (whitespaces+)
       (type)))
    (app (whitespaces*) (char ")")))))

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

(defun type ()
  (parser-header
   (alt
    (type-atom)
    (arrow))))

(defun type-atom ()
  (parser-header
   (alt
    (prefix "Integer")
    (prefix "Boolean"))))

(defun type-variadic ()
  (parser/map
   #'utils:add-variadic-tag/1
   (parser-header
    (justRight
     (char "&")
     (type)))))

(defun arrow ()
  (parser/map
   #'ast:make-arrow/1
   (parser-header
    (justRight
     (app (char "(") (whitespaces*))
     (justRight
      (app (prefix "->") (whitespaces*))
      (justLeft
       (app
	(justLeft (alt (type-variadic) (type)) (whitespaces+))
	(type))
       (app (whitespaces*) (char ")"))))))))

