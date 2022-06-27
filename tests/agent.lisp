(defpackage alaman/tests/agent
  (:import-from :alaman.agent)
  (:import-from :alaman.core)
  (:import-from :alaman.ns)
  (:local-nicknames
   (:agent :alaman.agent)
   (:ns :alaman.ns)
   (:core :alaman.core))
  (:use :cl :alaman :fiveam :alaman.core))
(in-package :alaman/tests/agent)

(def-suite* agent-tests
  :description "agent tests")

(in-suite agent-tests)

(defvar *ns* (ns:init))
(defvar *info* (make-agent-info
		:id "agent-id"
		:name "agent-name"
		:address "agent-addr"
		:settings '((root . "/tmp/test-agent"))))
(defvar *clock* (new-system-clock))
(defvar *agent* (agent:new-agent
		 :info *info*
		 :ns *ns*
		 :clock *clock*))

(test agent-start
  (agent:start *agent*)
  (is (ns:existsp *ns* "/agent/agent-name")))

;(run! 'agent-tests)

