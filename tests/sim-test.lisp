(defpackage alaman/tests/sim
  (:use #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.sim)
  (:import-from :alaman.core)
  (:import-from :alaman.command)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:local-nicknames
   (:admin :alaman.admin)
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:cmd :alaman.command)
   (:ns :alaman.ns)
   (:sim :alaman.sim)
   (:am :alaman.map))
  (:use :cl :fiveam :alaman.core))
(in-package :alaman/tests/sim)

(def-suite* sim-tests
  :description "sim tests")

(in-suite sim-tests)

(test sim-default-lifecycle
  (let ((S (sim:init)))
    (sim:start S)
    (sim:run-step S)
    (sim:run-step S)
    (sim:stop S)))

(test sim-init-args
  (let* ((spec (sim:make-spec :dims '(10 10)))
	 (clock (new-fixed-clock))
	 (nameserv (ns:init))
	 (agents (list (agent:init :info (core:make-agent-info :name "test")
				   :clock clock
				   :ns nameserv)))
	 (S (sim:init spec :agents agents)))
    (sim:start S)
    (sim:run-step S)
    (sim:stop S)))

(test sim-no-op
  (let ((S (sim:init (sim:make-spec)))
	(C (cmd:no-op)))
    (sim:start S)
    (sim:submit S C)
    (sim:run-step S)
    (is (equalp :done (core:cmd-state C)))
    (sim:stop S)))

(run! 'sim-tests)
