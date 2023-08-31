(defpackage kamys-utilities/classes
  (:nicknames :kamys-classes)
  (:use :cl)
  (:export class-factory make-default-slots))

(in-package :kamys-utilities/classes)

(defmacro class-factory (class-name
			 &key inherit slots initarg accessor)
;; add keys: function-prefix initform

  "CLASS-FACTORY macro creates a class with inheritance, and with slots
that can have accessor and initarg. 

ARGS:

CLASS-NAME: SYMBOL without quote 

KEYS:

INHERIT: LIST of SYMBOLS without quotes.
SLOTS: LIST of SYMBOLS without quotes.
INITARG: BOOLEAN, if T then add \":initarg slot-name\" in slot list.
ACCESSOR: BOOLEAN, if T then add \":accessor class-name-slot-names\"
in slot list.

CLASS-FACTORY creates a class with CLASS-NAME inheriting from classes
named in INHERIT and slots SLOTS.

_EXAMPLE_

> (class-factory class-1 :inherit (class-parent-1 class-parent-2) :slots (slot-1 slot-2))
#<STANDARD-CLASS COMMON-LISP-USER::CLASS-1>

> (describe 'class-1)
COMMON-LISP-USER::CLASS-1
  [symbol]

CLASS-1 names the standard-class #<STANDARD-CLASS COMMON-LISP-USER::CLASS-1>:
  Class precedence-list: CLASS-1, CLASS-PARENT-1, CLASS-PARENT-2,
                         STANDARD-OBJECT, SB-PCL::SLOT-OBJECT, T
  Direct superclasses: CLASS-PARENT-1, CLASS-PARENT-2
  No subclasses.
  Direct slots:
    SLOT-1
      Initargs: SLOT-1
      Readers: CLASS-1-SLOT-1
      Writers: (SETF CLASS-1-SLOT-1)
    SLOT-2
      Initargs: SLOT-2
      Readers: CLASS-1-SLOT-2
      Writers: (SETF CLASS-1-SLOT-2)

> (defvar class-1-instance (make-instance 'class-1 'slot-1 \"abc\" 'slot-2 42))
CLASS-1-INSTANCE

> (class-1-slot-1 class-1-instance)
\"abc\"

> (setf (class-1-slot-2 class-1-instance) 3.14159)
3.14159"
  (let ((slot-sub (eval `(make-default-slots ,class-name :slots ,slots
			     :initarg ,initarg :accessor ,accessor))))
    `(defclass ,class-name ,inherit ,slot-sub)))

(defmacro make-default-slots (class-name &key slots initarg accessor function-prefix)
  "Sometimes I have to make a class with a massive amount of slots and
 it is just easier to use this. This will output a list that you can
 copy paste into a defclass for the slots.

ARGS:

CLASS-NAME: SYMBOL without quote

KEYS:

SLOTS: LIST of SYMBOLS without quotes.
INITARG: BOOLEAN, if T then add \":initarg slot-name\" in slot list.
ACCESSOR: BOOLEAN, if T then add \":accessor class-name-slot-names\"
in slot list.

CLASS-FACTORY creates a class with CLASS-NAME inheriting from classes
named in INHERIT and slots SLOTS.

_EXAMPLE_

> (make-default-slots issue :slots (slot-1 slot-2) :initarg t :accessor t)
((SLOT-1 :INITARG SLOT-1 :ACCESSOR ISSUE-SLOT-1)
 (SLOT-2 :INITARG SLOT-2 :ACCESSOR ISSUE-SLOT-2))"
  (let* ((prefix (or function-prefix class-name))
	 (initarg-list (if initarg (list :initarg 'slot)))
	 (accessor-list (if accessor 
			    (list :accessor `(alexandria:make-keyword 
					      (concatenate 'string 
							   (string ',prefix)
							   "-"
							   (string slot)))))))
    `(loop for slot in ',slots
	   collect (list slot ,@initarg-list ,@accessor-list))))
