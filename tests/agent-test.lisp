(defpackage alaman/tests/agent
  (:use #:alaman.time)
  (:import-from :alaman.agent)
  (:import-from :alaman.core)
  (:import-from :alaman.command)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:local-nicknames
   (:agent :alaman.agent)
   (:ns :alaman.ns)
   (:core :alaman.core)
   (:cmd :alaman.command)
   (:am :alaman.map))
  (:use :cl :alaman :fiveam :alaman.core))
(in-package :alaman/tests/agent)

(def-suite* agent-tests
  :description "agent tests")

(in-suite agent-tests)

(test agent-names
  (is (not (null (agent:make-agent-name))))
  (is (not (null (agent:make-agent-names 5)))))

(defvar *ns* (ns:init))
(defvar *info* (make-agent-info :id "agent-id"
				:name "agent-name"
				:settings '((root . "/tmp/test-agent"))))
(defvar *clock* (new-system-clock))
(defvar *agent* (agent:init :info *info*
			    :ns *ns*
			    :clock *clock*))

(test agent-start
  (agent:start *agent*)
  (is (ns:existsp *ns* "/agent/agent-name"))
  (is (equal :active (agent:state *agent*))))

(test agent-stop
  (agent:start *agent*)
  (agent:stop *agent*)
  (is (equal :stopped (agent:state *agent*)))
  (is (equal :stopped (agent-info-state (ns:lookup *ns* "/agent/agent-name")))))

;; TODO: Check command execution
(test dostep-noop
  (agent:start *agent*)
  (agent:submit *agent* (make-command :id "test" :kind :no-op))
  (agent:dostep *agent*))

;; TODO: implement
(test rover-agent
  (let ((C (new-fixed-clock 0 0))
	(NS (ns:init))
	(tiles (am:uniform-map '(10 10) :kind :grass :climate :sunny))
	(U (make-universe :tiles tiles))
	(rover (agent:new-rover :info *info*
				:clock C
				:ns NS
				:universe U
				:location '(0 0)
				:max-speed 1))
	(move-cmd (cmd:move-to '(0 1))))

    (agent:start rover)

    (agent:submit rover (cmd:move-to '(0 1)))

    ;; Hasn't moved yet
    (agent:dostep *agent*)
    (is (equalp (agent:location rover) '(0 0)))

    ;; Now move
    (clock-set C 1)
    (agent:dostep *agent*)
    (is (equalp (command-state move-cmd) :done))
    (is (equalp (agent:location rover) '(0 1)))

    (agent:stop rover)))

(run! 'agent-tests)
