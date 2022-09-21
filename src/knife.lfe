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
		(make-seqlication 1)
		(make-abstraction 1)
		(make-arrow 1))
	  (from utils (add-variadic-tag 1))))

(defun parse (file-content)
  (case (funcall (get-parser (expression))
		 (build-input file-content))
    ((tuple input status result) (tuple status result))))

(defun expression ()
  (list-alt (list (condition) (seqlication) (abstraction) (literal) (variable))))

(defun variable ()
  (parser/map #'ast:make-variable/1 (parser/bind #'basic:invalid-lambda/1 (identifier))))

(defun literal ()
  (list-alt (list (any-number) (any-boolean))))

(defmacro parser-header (parser)
  `(build-parser
    `,(lambda (input)
	(funcall (get-parser ,parser) input))))

(defun seqlication ()
  (parser/map
   #'ast:make-seqlication/1
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
       (optional (variadic)))
     (seq (whitespaces*) (char "]"))))))

(defun variadic ()
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

(defun variadic-arg ()
  (parser-header
    (optional (justRight (char "&") (type)))))

(defun non-empty-arg ()
  (parser-header
    (many+
     (justLeft
      (type)
      (whitespaces*)))))

(defun args ()
  (parser-header
   (justRight
    (seq (char "(") (whitespaces*))
    (justLeft
     (seq (optional (non-empty-arg)) (variadic-arg))
     (seq (whitespaces*) (char ")"))))))

(defun arrow ()
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

