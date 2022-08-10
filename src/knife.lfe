(defmodule knife
  (export (main 1)))

(defun main ()
  nil)

(defmacro comment expr
  "&rest adds an extra parenthesis
   to the argument list, hece usage
   without (args ...)"
  nil)

;; run :: input<'T> -> (input<'U>, result<'U, 'TError>)
(defrecord parser
  run)

(defrecord input
  text)

(defun wrap (value)
  (make-parser run (lambda (input) (tuple input 'success value))))

(defun parser/map (f parser)
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
