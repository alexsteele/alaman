(defpackage alaman/tests/main
  (:use :cl
        :alaman
        :rove))
(in-package :alaman/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :alaman)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
