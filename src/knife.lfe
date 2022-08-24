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

;; (comment
;;  (let ((p (parser-run (parser/map (lambda (x) "DEF") (wrap "ABC")))))
;;    (funcall p (make-input text "123"))))

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

;; (comment
;;  (let ((p (parser-run ((wrap "ABC")))))
;;    (funcall p (make-input text "123"))))

(defun any-char ()
  (make-parser
   run
   (lambda (input)
     (case (input-text input)
       ("" (tuple input 'failure "No characters founds"))
       ((cons head tail) (tuple  (make-input text tail) 'success (list head)))))))

;; (defun *> (parserA parserB)
;;   "ParserA: Parser<'T>
;;    ParserB: Parser<'U>
;;    : Parser<'U>"
;;   (make-parser
;;    run
;;    (lambda (input)
;;      (let ((next (funcall (parser-run parserA) input)))
;;        (case next
;; 	 ((tuple new-input 'success value) (funcall (parser-run parserB) new-input)
;; 	  (tuple new-input 'failure message) (tuple new-input 'failure message)))))))

;; (defun <* (parserA parserB)
;;   "ParserA: Parser<'T>
;;    ParserB: Parser<'U>
;;    : Parser<'T>"
;;   (make-parser
;;    run
;;    (lambda (input)
;;      (let ((next (funcall (parser-run parserB) input)))
;;        (case next
;; 	 ((tuple new-input 'success value) (funcall (parser-run parserA) new-input)
;; 	  (tuple new-input 'failure message) (tuple new-input 'failure message)))))))

;; (defun <*> (parserA parserB)
;;   "ParserA: Parser<'T>
;;    ParserB: Parser<'U>
;;    : Parser<'T * 'U>")

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


;; (defun many (parser)
;;   "Parser: Parser<'T>
;;    : Parser<List<'T>>")