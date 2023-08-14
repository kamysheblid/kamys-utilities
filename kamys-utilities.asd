(defsystem "kamys-utilities"
  :version "0.1.0"
  :author "kamy"
  :license ""
  :description "Kamy's utilities"
  :in-order-to ((test-op (test-op "kamys utilities/tests"))))

(defsystem "kamys-utilities/strings"
  :author "kamy"
  :license ""
  :components ((:module "src"
                :components
                ((:file "strings"))))
  :description " Kamy's utilities for strings")

(defsystem "kamys-utilities/classes"
  :author "kamy"
  :license ""
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "classes"))))
  :description " Kamy's utilities for classes")

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
