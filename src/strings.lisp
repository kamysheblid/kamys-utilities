(defpackage kamys-string-utilities
  (:use :cl))
(in-package :kamys-string-utilities)

(defmacro string-concat (&rest strings)
  `(concatenate 'string @,strings))
