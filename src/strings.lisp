(defpackage kamys-utilities/strings
  (:use :cl)
  (:export string-concat))
(in-package :kamys-utilities/strings)

(defmacro string-concat (&rest strings)
  "Concatenate as many strings as you like.

STRINGS are simply strings.

_Example_

> (string-concat \"a\" \"b\" \"c\")
\"abc\""
  `(concatenate 'string ,@strings))
