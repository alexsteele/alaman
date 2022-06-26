(defpackage alaman/tests/ns
  (:import-from :alaman.ns)
  (:local-nicknames
   (:ns :alaman.ns))
  (:use :cl :alaman :fiveam))
(in-package :alaman/tests/ns)

(def-suite* ns-tests
  :description "nameserver tests")

(in-suite ns-tests)

(defvar *ns* (ns:init))

(test ns-basics
  (ns:register *ns* "/foo" "a" "b")
  (is (equal "b" (ns:lookup *ns* "/foo")))
  (is (equal "a" (ns:connect *ns* "/foo")))
  (is (equal nil (ns:children *ns* "/dne")))
  (is (equalp (list (ns:make-entry :name "/foo" :obj "a" :data "b"))
	      (ns:children *ns* "/"))))

; (run! 'ns-tests)
