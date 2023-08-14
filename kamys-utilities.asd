(defpackage "kamys-utilities"
  (:use #:cl #:asdf))

(in-package "kamys-utilities")

(defsystem "kamys-utilities"
  :version "0.1.0"
  :author "kamy"
  :license ""
  :depends-on (alexandria)
  :components ((:module "src"
		:components
		((:file "classes")
		 (:file "strings"))))
  :description "Kamy's utilities"
  :in-order-to ((test-op (test-op "kamys utilities/tests"))))


;; (defsystem "kamys-class-utilities"
;;   :author "kamy"
;;   :license ""
;;   :depends-on ("alexandria")
;;   :components ((:module "src"
;;                 :components
;;                 ((:file "classes.lisp"))))
;;   :description " Kamy's utilities for classes")

;; (defsystem "kamys-string-utilities"
;;   :author "kamy"
;;   :license ""
;;   :components ((:module "src"
;;                 :components
;;                 ((:file "strings.lisp"))))
;;   :description " Kamy's utilities for classes")

;; (defsystem "kamy's utilities"
;;   :version "0.1.0"
;;   :author "kamy"
;;   :license ""
;;   :depends-on ()
;;   :components ((:module "src"
;;                 :components
;;                 ((:file "main"))))
;;   :description "Kamy's utilities"
;;   :in-order-to ((test-op (test-op "kamy's utilities/tests"))))

;; (defsystem "kamy's utilities/tests"
;;   :author "kamy"
;;   :license ""
;;   :depends-on ("kamy's utilities"
;;                "rove")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "main"))))
;;   :description "Test system for kamy's utilities"
;;   :perform (test-op (op c) (symbol-call :rove :run c)))
