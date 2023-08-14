(defpackage kamys-utilities
  (:use :cl))
(in-package :kamys-utilities)

(defmacro string-concat (&rest strings)
  `(concatenate 'string ,@strings))
