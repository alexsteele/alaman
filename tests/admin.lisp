(defpackage alaman/tests/admin
  (:use #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.core)
  (:import-from :alaman.ns)
  (:local-nicknames
   (:admin :alaman.admin)
   (:core :alaman.core)
   (:ns :alaman.ns))
  (:use :cl :fiveam :alaman.core))
(in-package :alaman/tests/admin)

(def-suite* admin-tests
  :description "admin tests")

(in-suite admin-tests)

(defvar *ns* (ns:init))
(defvar *clock* (new-system-clock))
(defvar *admin* (admin:init
		 :folder "/tmp/admin"
		 :ns *ns*
		 :clock *clock*))

(test admin-start
  (ns:register *ns* "/agent/a0" nil (make-agent-info :id "0"))

  (admin:start *admin*)

  (is (ns:existsp *ns* "/admin"))
  (is (equalp (admin:list-agents *admin*) (list (make-agent-info :id "0")))))

; (run! 'admin-tests)
