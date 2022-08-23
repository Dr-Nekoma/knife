(defmodule knife
  (export (main 1)))

(defun main ()
  nil)

(defmacro comment expr
  "&rest adds an extra parenthesis
   to the argument list, hence usage
   without (args ...)"
  nil)

;; run :: input<'T> -> (input<'U>, result<'U, 'TError>)
(defrecord parser
  run)

(defrecord input
  text)

(defun wrap (value)
  (make-parser run (lambda (input) (tuple input 'success value))))

(defun zero ()
  (make-parser run (lambda (input) (tuple input 'failure nil))))

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

(comment
 (let ((p (parser-run (parser/map (lambda (x) "DEF") (wrap "ABC")))))
   (funcall p (make-input text "123"))))

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

(comment
 (let ((p (parser-run (parser/bind (lambda (x) (wrap "DEF")) (wrap "ABC")))))
   (funcall p (make-input text "123"))))

(defun satisfy (predicate)
  "Predicate: Char -> Boolean
   : Parser<String>"
  (make-parser
   run
   (lambda (input)
     (let ((condition (funcall predicate input)))
       (if condition
	 (tuple input 'success input)
	 (tuple input 'failure nil))))))

(defun match-char (ch)
  "Ch: Char
   : Parser<Char>"
  (satisfy (lambda (input) (== input ch)))) 

(defun match-word (word)
  "Word: String
   : Parser<String>"
  (let ((parsers (lists:map (lambda (x) (match-char x)) word)))
    (lists:fold (lambda (p acc) (join p acc)) (tuple (lists:nth 0 parsers) (lists:nth 0 word)) (lists:zip parsers word))))

(defun *> (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'U>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserA) input)))
       (case next
	 ((tuple new-input 'success value) (funcall (parser-run parserB) new-input)
	  (tuple new-input 'failure message) (tuple new-input 'failure message)))))))

(defun <* (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'T>"
  (make-parser
   run
   (lambda (input)
     (let ((next (funcall (parser-run parserB) input)))
       (case next
	 ((tuple new-input 'success value) (funcall (parser-run parserA) new-input)
	  (tuple new-input 'failure message) (tuple new-input 'failure message)))))))

(defun <*> (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'U>
   : Parser<'T * 'U>")

(defun <|> (parserA parserB)
  "ParserA: Parser<'T>
   ParserB: Parser<'T>
   : Parser<'T>")

(defun ?? (parser)
  "Parser: Parser<'T>
   : Parser<Option<'T>>")

(defun many (parser)
  "Parser: Parser<'T>
   : Parser<List<'T>>")