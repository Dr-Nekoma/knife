(defmodule basic
  (export (many+ 1)
	  (many* 1)
	  (char 1)
	  (any-number 0)
	  (justLeft 2)
	  (justRight 2)
	  (build-parser 1)
	  (build-input 1)
	  (get-parser 1)
	  (list-alt 1)
	  (whitespace 0)
	  (app 2)
	  (alt 2)
	  (optional 1)
	  (variadic 0)
	  (identifier 0)
	  (while 1)
	  (prefix 1)
	  (predicate-whitespace 1)
	  (any-char 0)
	  (any-boolean 0)
	  (whitespaces* 0)
	  (whitespaces+ 0)
	  (parser/map 2)))

(defrecord parser
  run)

(defun get-parser (parser)
  (parser-run parser))

(defrecord input
  text
  position)

(defun empty ()
  (make-parser run (lambda (input) (tuple input 'success '()))))

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
			    (tuple (make-input text tail) 'success (tuple 'literal (tuple 'integer (list head))))
			    (tuple input 'failure "No digits were found")))))))

(defun check-boolean (candidate) (or (=:= candidate 84) (=:= candidate 70)))

(defun any-boolean ()
  (make-parser
   run
   (lambda (input)
     (case (input-text input)
       ("" (tuple input 'failure "No characters were found"))
       ((cons head tail) (if (check-boolean head)
			    (tuple (make-input text tail) 'success (tuple 'literal (tuple 'boolean (list head))))
			    (tuple input 'failure "No booleans were found")))))))

(defun check-char (chr1 chr2 input new-input)
  (if (=:= chr1 chr2)
    (tuple new-input 'success chr1)
    (tuple input 'failure "Didn't find specific char")))

(defun whitespace ()
  (list-alt (list (char " ") (char "\n") (char "\t") (char "\r\t") (char "\r\n"))))

(defun whitespaces+ ()
  (many+ (whitespace)))

(defun whitespaces* ()
  (many* (whitespace)))

(defun predicate-whitespace (chr)
  (or (=:= chr (car " ")) (=:= chr (car "\n")) (=:= chr (car "\t"))))
 
(defun identifier ()
  (while (lambda (chr) (not (predicate-whitespace chr)))))

(defun variadic ()
 (app (char "&") (identifier)))

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
	 ((tuple _ 'failure _) (funcall (parser-run parserB) input)))))))

(defun list-alt (parsers)
  (let ((rev-parsers (lists:reverse parsers)))
    (lists:foldl #'alt/2 (car rev-parsers) (cdr rev-parsers))))

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

(defun many+ (parser)
  "Parser: Parser<'T>
  : Parser<List<'T>>"
  (make-parser
   run
   (lambda (input)
     (fletrec ((loop (input parser acc nth)
                   (case (funcall (parser-run parser) input)
                     ((tuple new-input 'success value) (funcall #'loop/4 new-input parser (cons value acc) (+ 1 nth)))
                     ((tuple new-input 'failure message) (tuple new-input acc nth)))))
       (case (loop input parser '() 0)
	 ((tuple _ _ 0) (tuple input 'failure "Couldn't parse using many+"))
         ((tuple final-input acc _) (tuple final-input 'success (lists:reverse acc))))))))

(defun many* (parser)
  (alt (many+ parser) (empty)))

(defun optional (parser)
  (alt parser (empty)))

;; let prefix (prefix_str: string): string parser =
;;   { run = fun input ->
;;           let unexpected_prefix_error =
;;             Printf.sprintf "expected `%s`" prefix_str
;;           in
;;           try
;;             let prefix_size = String.length prefix_str in
;;             let input_size = String.length input.text in
;;             let prefix_input = input |> input_sub 0 prefix_size in
;;             if String.equal prefix_input.text prefix_str then
;;               let rest = input |> input_sub prefix_size (input_size - prefix_size) in
;;               rest, Ok prefix_str
;;             else
;;               input, Error unexpected_prefix_error
;;           with
;;             Invalid_argument _ -> input, Error unexpected_prefix_error
;;   }

(defun prefix (str)
  (make-parser
   run
   (lambda (input)
     (case (string:prefix (input-text input) str)
       ('nomatch (tuple input 'failure "Couldn't find prefix"))
       (rest (tuple (build-input rest) 'success str))))))
