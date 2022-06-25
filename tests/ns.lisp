(defpackage alaman/tests/ns
  (:import-from :alaman.ns)
  (:local-nicknames
   (:ns :alaman.ns))
  (:use :cl :alaman :rove))
(in-package :alaman/tests/ns)

(defvar *ns* (ns:init))

;; TODO: Get this working
(deftest test-ns
  (testing "basics"
    (ns:register *ns* "/foo" "a" "b")
    (ok (equal "b" (ns:lookup *ns* "/foo")))
    (ok (equal  "a" (ns:connect *ns* "/foo")))
    (ok (equal nil (ns:children *ns* "/dne")))
    (ok (equal (list ns::make-entry :name "/foo" :obj "a" :data "b")
	       (ns:children *ns* "/")))))
