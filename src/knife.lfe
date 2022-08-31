(defmodule knife
  (export (main 0)))

(defun main ()
  '())

(defmacro comment expr
  "&rest adds an extra parenthesis
   to the argument list, hence usage
   without (args ...)"
  '())

;; run :: input<'T> -> (input<'U>, result<'U, 'TError>)
(defrecord parser
  run)

(defrecord input
  text
  position)

(defun wrap (value)
  (make-parser run (lambda (input) (tuple input 'success value))))

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

;;(comment
;;  (let ((p (parser-run ((wrap "ABC")))))
;;    (funcall p (make-input text "123"))))

(defun any-char ()
  (make-parser
   run
   (lambda (input)
     (case (input-text input)
       ("" (tuple input 'failure "No characters founds"))
       ((cons head tail) (tuple  (make-input text tail) 'success (list head)))))))

(defun app (parserA, parserB)
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

(defun justLeft (parserA, parserB)
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

(defun justRight (parserA, parserB)
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

(defun alt (parserA, parserB)
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
            (splitted-content (string:split predicate content))
            (new-input (second splitted-content))
            (value (first splitted-content)))
       (tuple new-input 'success value)))))

; let prefix (prefix_str: string): string parser =
;   { run = fun input ->
;           let unexpected_prefix_error =
;             Printf.sprintf "expected `%s`" prefix_str
;           in
;           try
;             let prefix_size = String.length prefix_str in
;             let input_size = String.length input.text in
;             let prefix_input = input |> input_sub 0 prefix_size in
;             if String.equal prefix_input.text prefix_str then
;               let rest = input |> input_sub prefix_size (input_size - prefix_size) in
;               rest, Ok prefix_str
;             else
;               input, Error unexpected_prefix_error
;           with
;             Invalid_argument _ -> input, Error unexpected_prefix_error
;   }

(defun many (parser)
  "Parser: Parser<'T>
  : Parser<List<'T>>"
  (make-parser
   run
   (lambda (input)
      (flet ((loop (input parser acc)
                (case (funcall (parser-run parser) input)
                  ((tuple new-input 'success value) (loop new-input parser (cons value acc)))
                  ((tuple new-input 'failure message) (tuple new-input acc)))))
        (case (loop input parser '())
          ((tuple final-input acc) (tuple final-input 'success (lists:reverse acc))))))))

; (let ((p (parser-run (many (any-char)))))
;   (funcall p (make-input text "aaaaa")))

; (parser-run (many (any-char)) (make-input text "aaaaa")) 