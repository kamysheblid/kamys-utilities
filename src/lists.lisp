(defpackage kamys-utilities/lists
  (:nicknames :kamys-lists)
  (:use #:cl)
  (:export #:range #:insert-to-nth #:nthcars #:filter))

(in-package :kamys-utilities/lists)

(defun nthcars (n lst)
  "Like NTHCDR, but instead it returns a list containg the first n
elements of LST.

_EXAMPLE_

> (kamys-utilities/lists::nthcars 5 '(1 2 3 4 5))
;; => (1 2 3 4 5)
> (kamys-utilities/lists::nthcars 2 '(1 2 3 4 5))
;; => (1 2 3)"
  (if (> n (length lst))
      (error "N (~A) is larger than the length of the list: ~A" n (length lst))
      (loop for i from 0 below n
	    collect (nth i lst))))

(defun insert-to-nth (lst elt n)
  "Insert an ELT in position N of LST. 

It can also take negative values. The negative values are handled in a
way so that the last element can be accessed, it is sort of counting
the nil value at the end of every list.

To access the first element using negative values use the negative of
the length of the list but less one. So if the length is 10, then we
use -11 to access the first element.

_EXAMPLE_

> (insert-to-nth (range 10) 'a -1)
;; => (0 1 2 3 4 5 6 7 8 9 A)
> (insert-to-nth (range 10) 'a -10)
;; => (0 A 1 2 3 4 5 6 7 8 9)
> (insert-to-nth (range 10) 'a -11)
;; => (A 0 1 2 3 4 5 6 7 8 9)

_NOTE_

I created this function for the threading macro in kamys-macros. The
code is more legible and understandable with this function instead of
using cars and cdrs. That is why I made the negative values a bit
weird, because I needed to be able to push ELT to the end of the list."
  (cond ((or (> n (length lst)) (< n (- -1 (length lst))))
	 ;; N cannot be larger than the list, handle negative N's as
	 ;; well.
	 (error
	  "The list is ~A elements long, so cannot put anything in position ~A"
	  (length lst) n))
	;; If N is negative check if its valid and set to its
	;; equivalent positive integer.
	((and (< n 0))
	 (setf n (+ (length lst) n 1))))
  (append (nthcars n lst) (list elt) (nthcdr n lst)))

(defun range (range &optional stop step)
  "Supposed to be like python's range function. Give it 1 arg and it will
return a list from 0 until RANGE. Give it 2 args and it will return a
list from RANGE until (but not including) stop. Give it 3 args and it
will return a list going from RANGE until STOP in intervals of STEP.

_EXAMPLE_

> (range 5)
;; => (0 1 2 3 4)
> (range 2 5)
;; => (2 3 4)
> (range 2 5 2)
;; => (2 4)"
  (cond ((and (not stop) (not step))
	 (progn (setq stop range)
		(setq range 0)
		(setq step 1)))
	((not step)
	 (setq step 1)))
  (loop for i from range below stop by step
	collect i))

(defun filter (pred lst)
  "Build new list using elements from LST that eval to T when applied to
PRED.

_EXAMPLE_

> (filter (lambda (x) (not (= x 1))) '(1 2 3))
;; => (2 3)
> (filter (lambda (x) (= x 0)) '(1 2 3))
;; => NIL"
  (cond ((null lst) lst)
	((funcall pred (car lst))
	 (cons (car lst) (filter pred (cdr lst))))
	(t
	 (filter pred (cdr lst)))))
