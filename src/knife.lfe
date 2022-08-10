(defmodule knife
  (export (main 1)))

(defun main ()
  nil)

;; (tuple "" 'success value)
;; (tuple "" 'failure message)

;; This is the raw input from the .sw file
(defrecord parser/input
  text
  position)

;; This is the error message that the parser will generate
(defrecord parser/error
  message
  position)

;; This function picks an input a parser and returns a new parser with a bound input
(defrecord parser
  runParser)

;; Functor
;; Function: a -> b
;; Parser: a
;; Parser b
(defun parser/map (f parser)
  (make-parser
   runParser 
   (lambda (input)
     (let ((output (funcall (runParser parser) input)))
       (case output
	 ((new-input 'success value) (tuple new-input 'success (funcall #'f/1 value)))
	 ((new-input 'failure message) (tuple new-input 'failure message)))))))

(defun test-fun (input)
  (string:slice input 2))

(set test-input ((make-parser/input text "abcdef" position 0) 'sucesss 2))
(set test-mapped-parser (parser/map #'test-fun/1 test-input))