(defmodule basic
  (export (many 1)
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
	  (parser/map 2)))

(defrecord parser
  run)

(defun get-parser (parser)
  (parser-run parser))

(defrecord input
  text
  position)

(defun wrap (value)
  (make-parser run (lambda (input) (tuple input 'success value))))

(defun build-parser (lamb)
  (make-parser
   run
   lamb))

(defun build-input (content)
  (make-input
   text
   content))

(defun parser/map (f parser)
  "F: 'T -> 'U
   Parser: Parser<'T>
   : Parser<'U>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parser) input)))
       (case next
	 ((tuple new-input 'success value) (tuple new-input 'success (funcall f value)))
	 ((tuple new-input 'failure message) (tuple new-input 'failure message)))))))

(defun parser/bind (f parser)
  "F: 'T -> Parser<'U>
   Parser: Parser<'T>
   : Parser<'U>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parser) input)))
       (case next
	 ((tuple new-input 'success value) (funcall (parser-run (funcall f value)) new-input))
	 ((tuple new-input 'failure message) (tuple new-input 'failure message)))))))

(defun check-number (candidate) (and (>= candidate 48) (=< candidate 57)))

(defun any-number ()
  (make-parser
   run
   (lambda (input)
     (case (input-text input)
       ("" (tuple input 'failure "No characters were found"))
       ((cons head tail) (if (check-number head)
			    (tuple (make-input text tail) 'success (list head))
			    (tuple input 'failure "No digits were found")))))))

(defun check-char (chr1 chr2 input new-input)
  (if (=:= chr1 chr2)
    (tuple new-input 'success chr1)
    (tuple input 'failure "Didn't find specific char")))

(defun space ()
  (char " "))

(defun predicate-whitespace (chr)
  (or (=:= chr (car " ")) (=:= chr (car "\n")) (=:= chr (car "\t"))))
 
(defun identifier ()
  (justRight (many (space)) (while (lambda (chr) (not (predicate-whitespace chr))))))

(defun char (chr1)
  (make-parser
   run
   (lambda (input)
     (let ((output (funcall (parser-run (any-char)) input)))
       (case output
	 ((tuple new-input 'success chr2) (check-char chr1 chr2 input new-input))
	 ((tuple new-input 'failure message) (tuple new-input 'failure message)))))))

(defun any-char ()
  (make-parser
   run
   (lambda (input)
     (case (input-text input)
       ("" (tuple input 'failure "No characters were found"))
       ((cons head tail) (tuple  (make-input text tail) 'success (list head)))))))

(defun app (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'T * 'U>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserA) input)))
       (case next
	 ((tuple first-input 'success first-value)
	  (let ((next-next (funcall (parser-run parserB) first-input)))
	    (case next-next
	      ((tuple second-input 'success second-value) (tuple second-input 'success (tuple first-value second-value)))
	      ((tuple second-input 'failure message) (tuple second-input 'failure message)))))
	 ((tuple first-input 'failure message) (tuple first-input 'failure message)))))))

(defun justLeft (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'T>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserA) input)))
       (case next
	 ((tuple first-input 'success first-value)
	  (let ((next-next (funcall (parser-run parserB) first-input)))
	    (case next-next
	      ((tuple second-input 'success second-value) (tuple second-input 'success first-value))
	      ((tuple second-input 'failure message) (tuple second-input 'failure message)))))
	 ((tuple first-input 'failure message) (tuple first-input 'failure message)))))))

(defun justRight (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'U>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserA) input)))
       (case next
	 ((tuple first-input 'success first-value)
	  (let ((next-next (funcall (parser-run parserB) first-input)))
	    (case next-next
	      ((tuple second-input 'success second-value) (tuple second-input 'success second-value))
	      ((tuple second-input 'failure message) (tuple second-input 'failure message)))))
	 ((tuple first-input 'failure message) (tuple first-input 'failure message)))))))

(defun alt (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'T>
   : Parser<'T>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserA) input)))
       (case next
	 ((tuple new-input 'success value) (tuple new-input 'success value))
	 ((tuple new-input 'failure message) (funcall (parser-run parserB) input)))))))

(defun list-alt (parsers)
  (lists:foldl #'alt/2 (car parsers) (cdr parsers)))

(defun predTest (char)
  (=:= char 97))

(defun first (value)
  (case value
    ((tuple head tail) head)))

(defun separate (obj)
  (lists:splitwith (lambda (x) (=:= x "a")) obj))

(defun second (value)
  (case value
    ((tuple head tail) tail)))

(defun while (predicate)
  "Predicate: ('T -> bool)
   : Parser<seq<'T>>"
  (make-parser
   run
   (lambda (input)
     (let* ((content (input-text input))
	    (splitted-content (lists:splitwith predicate content))
            (new-input (second splitted-content))
            (value (first splitted-content)))
       (tuple (build-input new-input) 'success value)))))

(defun many (parser)
  "Parser: Parser<'T>
  : Parser<List<'T>>"
  (make-parser
   run
   (lambda (input)
     (fletrec ((loop (input parser acc)
                   (case (funcall (parser-run parser) input)
                     ((tuple new-input 'success value) (funcall #'loop/3 new-input parser (cons value acc)))
                     ((tuple new-input 'failure message) (tuple new-input acc)))))
       (case (loop input parser '())
         ((tuple final-input acc) (tuple final-input 'success (lists:reverse acc))))))))
