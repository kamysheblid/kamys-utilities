(defpackage kamys-utilities/macros
 (:nicknames :kamys-macros)
  (:use :cl)
  (:import-from :alexandria-1 :flatten)
  (:import-from :kamys-utilities/lists #:insert-to-nth)
  (:export #:-> #:-< #:defmacro/g! #:with-env))

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

(defmacro with-env (new-env &body body)
  "Lexical enviornment variables. Use it just like a let form.

Lets you run code with temporarily set environment variables. For
example: dexedor is an http client, it checks the environment to see
if HTTPS_PROXY or HTTP_PROXY is set, if it is then it will use the
proxy. If it isnt then it will continue as normal.

It works using a 4 step process. Steps:
1. Store old values in env-variable class
2. Set new vals using loop
3. Run code body
4. Set vars back to old vals or unset if old val was nil

NOTE: due to using sb-posix, this only works for sbcl

_EXAMPLE_

> (with-env ((\"VAR\" \"VAL\")
             (\"PATH\" \"\")) 
            (format t \"VAR: ~A~%\" (uiop:getenv \"VAR\"))
            (format t \"PATH: ~A~%\" (uiop:getenv \"PATH\")))
VAR: VAL
PATH: 
NIL

> (format t \"VAR: ~A~%\" (uiop:getenv \"VAR\"))
VAR: NIL
NIL
> (format t \"PATH: ~A~%\" (uiop:getenv \"PATH\"))
PATH: /usr/bin
NIL"
  (let ((env '()))
    ;; Step 1: store old values in ENV var
    (loop for (var-name new-val) in new-env
	  do (push (make-env-variable var-name 
				      (uiop:getenv var-name)
				      new-val)
		   env))
    ;; Step 2: set new vals
    (loop for var in env
	  do (set-env-variable-new-val var))
    ;; Step 3: run body with env vars
    (mapcar #'eval body)
    ;; Step 4: set vars back to old or unset if nil
    (loop for var in env
	  do (set-env-variable-old-val var))))

(defclass env-variable ()
  ((name :initarg :name :accessor get-env-var-name)
   (old-value :initarg :old-value :accessor get-env-old-value)
   (new-value :initarg :new-value :accessor get-env-new-value)))

(defun make-env-variable (name old-val new-val)
  (make-instance 'env-variable
		 :name name
		 :old-value old-val
		 :new-value new-val))

(defun set-env-variable-new-val (env-var)
  (let ((env-var-name (get-env-var-name env-var))
	(env-new-value (get-env-new-value env-var)))
    (setf (uiop:getenv env-var-name) env-new-value)))

(defun set-env-variable-old-val (env-var)
  (let* ((env-var-name (get-env-var-name env-var))
	 (env-old-val (get-env-old-value env-var)))
    (if env-old-val
	(setf (uiop:getenv env-var-name)
	      env-old-val)
	#+sbcl
	(sb-posix:unsetenv env-var-name)
	#-sbcl
	(setf (uiop:getenv env-var-name) env-old-val))))
