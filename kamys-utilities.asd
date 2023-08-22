(defsystem "kamys-utilities"
  :version "0.1.0"
  :author "kamy"
  :license ""
  :description "Kamy's utilities"
  :in-order-to ((test-op (test-op "kamys utilities/tests")))
  :depends-on ("alexandria")
  :components ((:module "src"
		:components
		((:file "strings")
		 (:file "classes")
		 (:file "lists")
		 (:file "macros")))))

(defsystem "kamy's utilities/tests"
  :author "kamy"
  :license ""
  :depends-on ("kamy's utilities"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for kamy's utilities"
  :perform (test-op (op c) (symbol-call :rove :run c)))
