(defpackage kamys-utilities/macros
 (:nicknames :kamys-macros)
  (:use :cl)
  (:import-from :alexandria-1 :flatten)
  (:import-from :kamys-utilities/lists #:insert-to-nth)
  (:export #:-> #:-< #:defmacro/g!))

(in-package :kamys-utilities/macros)

(defmacro -< (&body body)
  "Threading macro which takes a form and inserts it as the first
arg of the next form.

_EXAMPLE_

> (-< (+ 1 1)
      (+ 2))
;; => 4

Whats happening is that it takes (+ 1 1) and puts it at the start of
the next form to give us (+ (+ 1 1) 2)."
  (let ((fixed-body `(thread-helper ',body 1)))
    `(eval ,fixed-body)))

(defmacro -> (&rest body)
  "Threading macro which takes a form and inserts it as the last
arg of the next form.

_EXAMPLE_

> (-> (+ 1 1)
      (+ 2))
;; => 4

It is using thread-helper to take (+ 1 1), place it at the end of the
next form to give us (+ 2 (+ 1 1))."
  (let ((fixed-body `(thread-helper ',body -1)))
    `(eval ,fixed-body)))

(defun thread-helper (sexp n)
  "Fixes s-expressions so they can be evaluated by threading macros. It
takes a list containing all of the forms and then attaches the first
one to the nth position of the second one, and then the modified
second one to the nth position of the third one, and on and on until
we have a single s-expression.

N can be negative.

_EXAMPLE_

> (kamys-utilities/macros::thread-helper '((+ 1 1) (+ 2 2) (+ 3 3)) -1)
;; => (+ 3 3 (+ 2 2 (+ 1 1)))"
  (cond ((null (cdr sexp)) (car sexp)) ;;im not sure what to return here
	((atom (car sexp)) sexp)
	((consp (cadr sexp))
	 (thread-helper (append 
			 (list 
			  (insert-to-nth
			   (cadr sexp) (car sexp) n))
			 (cddr sexp))
			n))))

;;; macros from "Let Over Lambda" Book by Hoyte
(defun g!-symbol-p (s)
  "From page 59 of LOL by Hoyte. Checks if s SYMBOL is a g! symbol."
	   (and (symbolp s)
		(> (length (symbol-name s)) 2)
		(string= (symbol-name s)
			 "G!"
			 :start1 0
			 :end1 2)))

(defmacro defmacro/g! (name args &rest body)
  "From page 60 of LOL by Hoyte. It is a defmacro that avoids variable
 capture. Simply use it as you would use defmacro."
  (let ((syms (remove-duplicates
	       (remove-if-not #'g!-symbol-p
			      (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar (lambda (s)
		       `(,s (gensym ,(subseq
				      (symbol-name s)
				      2))))
	      syms)
	 ,@body))))
