(defmodule basic
  (export (many+ 1)
	  (many* 1)
	  (char 1)
	  (any-number 0)
	  (justLeft 2)
	  (justRight 2)
	  (build-parser 1)
	  (get-parser 1)
	  (list-alt 1)
	  (seq 2)
	  (alt 2)
	  (invalid-lambda 1)
	  (optional 1)
	  (identifier 0)
	  (prefix 1)
	  (any-boolean 0)
	  (whitespaces* 0)
	  (whitespaces+ 0)
	  (parser/map 2)
	  (parser/bind 2))
  (import (from utils (string-to-integer 1)
		(string-to-boolean 1))))

(defrecord parser
  run)

(defun get-parser (parser)
  (parser-run parser))

(defun empty ()
  (make-parser run (lambda (input) (tuple input 'success '()))))

(defun build-parser (lamb)
  (make-parser
   run
   lamb))

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

(defun predicate-number (candidate) (and (>= candidate 48) (=< candidate 57)))

(defun any-number ()
  (parser/map
   #'utils:string-to-integer/1
   (justRight
    (empty-str)
    (while (lambda (char) (predicate-number char))))))

(defun any-digit ()
  (make-parser
   run
   (lambda (input)
     (case input
       ("" (tuple input 'failure (tuple  "No characters were found in ~p~n" `,(list input))))
       ((cons head tail) (if (predicate-number head)
			    (tuple tail 'success (tuple 'literal (tuple 'integer (utils:string-to-integer (list head)))))
			    (tuple input 'failure  (tuple "No digits were found in ~p~n" `,(list input)))))))))

(defun predicate-boolean (candidate) (or (=:= candidate 84) (=:= candidate 70)))

(defun any-boolean ()
  (make-parser
   run
   (lambda (input)
     (case input
       ("" (tuple input 'failure (tuple  "No characters were found in ~p~n" `,(list input))))
       ((cons head tail) (if (predicate-boolean head)
			    (tuple tail 'success (tuple 'literal (tuple 'boolean (utils:string-to-boolean (list head)))))
			    (tuple input 'failure (tuple  "No booleans were found in ~p~n" `,(list input)))))))))

(defun whitespace ()
  (list-alt (list (char " ") (char "\n") (char "\t") (char "\r\t") (char "\r\n"))))

(defun whitespaces+ ()
  (many+ (whitespace)))

(defun whitespaces* ()
  (many* (whitespace)))

(defun not-predicate-whitespace (chr)
  (not (or (=:= chr (car " ")) (=:= chr (car "\n")) (=:= chr (car "\t")))))

(defun not-variadic-lock (chr)
  (not (=:= chr (car "&"))))

(defun not-delimiters (chr)
  (not (or (=:= chr (car "[")) (=:= chr (car "]")))))

(defun invalid-lambda (output)
  (make-parser
   run
   (lambda (input)
     (case output
       ("lambda" (tuple input 'failure (tuple  "Found lambda as a variable name in ~p~n" `,(list input))))
       (_ (tuple input 'success output)))))) 

(defun empty-str ()
  (make-parser
   run
   (lambda (input)
     (case input
       ("" (tuple input 'failure (tuple  "No characters were found in ~p~n" `,input)))
       (content (tuple content 'success '()))))))

(defun invalid-chr (str)
  (make-parser
   run
   (lambda (input)
       (if (=:= str (lists:nth 1 input))
	   (tuple input 'failure (tuple  "Invalid character ~p encountered in ~p~n" `,(list str input)))
	 (tuple input 'success '())))))

(defun identifier ()
  (justRight
    (empty-str)
    (justRight
     (seq (invalid-chr (car "&")) (seq (invalid-chr (car "[")) (invalid-chr (car "]"))))
     (while (lambda (chr) (and (not-predicate-whitespace chr) (not-variadic-lock chr) (not-delimiters chr)))))))

(defun check-char (chr1 chr2 input new-input)
  (if (=:= chr1 chr2)
    (tuple new-input 'success chr1)
    (tuple input 'failure (tuple  "Didn't find specific char ~p in ~p~n" `,(list chr1 input)))))

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
     (case input
       ("" (tuple input 'failure (tuple  "No characters were found in ~p~n" `,(list input))))
       ((cons head tail) (tuple tail 'success (list head)))))))

(defun seq (parserA parserB)
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
	      ((tuple second-input 'failure message) (tuple input 'failure message)))))
	 ((tuple first-input 'failure message) (tuple input 'failure message)))))))

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

(defun while (predicate)
  "Predicate: ('T -> bool)
   : Parser<seq<'T>>"
  (make-parser
   run
   (lambda (input)
     (let* ((splitted-content (lists:splitwith predicate input))
            (new-input (tref splitted-content 2))
            (value (tref splitted-content 1)))
       (tuple new-input 'success value)))))

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
	 ((tuple _ _ 0) (tuple input 'failure (tuple  "Couldn't parse using many+ in ~p~n" `,(list input))))
         ((tuple final-input acc _) (tuple final-input 'success (lists:reverse acc))))))))

(defun many* (parser)
  (alt (many+ parser) (empty)))

(defun optional (parser)
  (alt parser (empty)))

(defun prefix (str)
  (make-parser
   run
   (lambda (input)
     (case (string:prefix input str)
       ('nomatch (tuple input 'failure (tuple "Couldn't find prefix ~p in ~p~n" `,(list str input))))
       (rest (tuple rest 'success str))))))
