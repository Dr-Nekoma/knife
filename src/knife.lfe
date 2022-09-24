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
		(seq 2)
		(identifier 0)
		(alt 2)
		(prefix 1)
		(optional 1)
		(invalid-lambda 1)
		(any-boolean 0)
		(parser/map 2)
		(parser/bind 2)
		(whitespaces* 0)
		(whitespaces+ 0))
	  (from ast (make-variable 1)
		(make-condition 1)
		(make-application 1)
		(make-abstraction 1)
		(make-arrow 1))
	  (from utils (get-second 1))))

(defun parse (file-content)
  (case (funcall (get-parser (expression)) file-content)
    ((tuple input 'success result) (tuple input 'success result))
    ((tuple input 'failure (tuple err-message args)) (funcall #'io:format/2 (string:concat "KNIFE| " err-message) args))))

(defun expression ()
  (list-alt (list (application) (abstraction) (condition) (literal) (variable))))

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
     (seq (char "[") (whitespaces*))
     (justLeft
      (seq (expression) (many* (justRight (whitespaces+) (expression))))
      (seq (whitespaces*) (char "]")))))))

(defun parameters ()
  (parser-header
   (justRight
    (seq (char "[") (whitespaces*))
    (justLeft
     (seq
      (many*
       (justLeft
	(seq
	 (justRight
	  (seq (whitespaces*) (char "("))
	  (justRight (whitespaces*) (identifier)))
	 (justLeft
	  (justRight (whitespaces+) (type))
	  (seq (whitespaces*) (char ")"))))
	(whitespaces*)))
       (optional (typed-variadic)))
     (seq (whitespaces*) (char "]"))))))

(defun typed-variadic ()
  (justRight
   (char "&")
   (justLeft
    (seq
     (justRight
      (char "(")
      (identifier))
     (justRight
       (whitespaces+)
       (type)))
    (seq (whitespaces*) (char ")")))))

(defun abstraction ()
  (parser/map
   #'ast:make-abstraction/1
   (parser-header
    (justRight
     (seq (char "[") (whitespaces*))
     (justLeft
      (seq
       (justRight (justLeft (prefix "lambda") (whitespaces*)) (parameters))
       (many+ (justRight (whitespaces+) (expression))))
      (seq (whitespaces*) (char "]")))))))

(defun condition ()
  (parser/map
   #'ast:make-condition/1
   (parser-header
    (justRight
     (seq (char "[") (whitespaces*))
     (justLeft
      (seq
       (justRight (seq (prefix "if") (whitespaces+)) (expression))
       (seq (justRight (whitespaces+) (expression)) (justRight (whitespaces+) (expression))))
      (seq (whitespaces*) (char "]")))))))

(defun type ()
  (parser-header
   (alt
    (type-atom)
    (type-arrow))))

(defun type-atom ()
  (parser-header
   (alt
    (prefix "Integer")
    (prefix "Boolean"))))

(defun variadic-arg ()
  (parser-header
   (justRight
    (seq (whitespaces+) (char "&"))
    (type))))

(defun non-empty-arg ()
  (flet ((get-types (list-t)
		    (lists:map #'utils:get-second/1 list-t)))
    (parser/map
     #'get-types/1
     (parser-header
      (many+
       (seq
	(whitespaces*)
	(type)))))))

(defun args ()
  (parser-header
   (justRight
    (char "(")
    (justLeft
     (seq (optional (non-empty-arg)) (optional (variadic-arg)))
     (seq (whitespaces*) (char ")"))))))

(defun type-arrow ()
  (parser/map
   #'ast:make-arrow/1
   (parser-header
    (justRight
     (seq (char "(") (whitespaces*))
     (justRight
      (seq (prefix "->") (whitespaces*))
      (justLeft
       (seq
	(justLeft (args) (whitespaces*))
	(type))
       (seq (whitespaces*) (char ")"))))))))

