(defpackage kamy's utilities/tests/main
  (:use :cl
        :kamy's utilities
        :rove))
(in-package :kamy's utilities/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :kamy's utilities)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
