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

(test agent-start-stop
    (let* ((info (make-agent-info :id "agent-id" :name "agent-name"))
	   (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	   (NS (ns:init))
	   (A (agent:init :info info :ns NS :clock clock)))

      (agent:start A)
      (is (ns:existsp NS "/agent/agent-name"))
      (is (equal :active (agent:state A)))

      (agent:stop A)
      (is (equal :stopped (agent:state A)))
      (is (equal :stopped (agent-info-state (ns:lookup NS "/agent/agent-name"))))))

(test dostep-no-commands
  (let* ((info (make-agent-info :id "agent-id" :name "name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock)))
    (agent:start A)
    (clock-set clock 10)
    (agent:dostep A)
    (agent:stop A)))

(test no-op-command
  (let* ((info (make-agent-info :id "agent-id" :name "name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock))
	 (no-op (cmd:no-op)))
    (agent:start A)
    (agent:submit A no-op)
    (clock-set clock 10)
    (agent:dostep A)

    (is (equalp :done (core:command-state no-op)))

    (agent:stop A)))

(test sleep-command
  (let* ((info (make-agent-info :id "agent-id" :name "name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock))
	 (sleep-cmd (cmd:sleep-until 20)))

    ;; Submit at T=0
    (agent:start A)
    (agent:submit A sleep-cmd)

    ;; Step to T=10
    (clock-set clock 10)
    (agent:dostep A)
    (is (equalp :running (core:command-state sleep-cmd)))
    (is (equalp :sleeping (agent:state A)))

    ;; Step to T=20. Finish.
    (clock-set clock 20)
    (agent:dostep A)
    (is (equalp :done (core:command-state sleep-cmd)))
    (is (equalp :active (agent:state A)))

    (agent:stop A)))

;; TODO: implement
(defun DISABLED-rover-agent ()
  (let ((clock (new-fixed-clock 0 0))
	(NS (ns:init))
	(tiles (am:uniform-map '(10 10) :kind :grass :climate :sunny))
	(U (make-universe :tiles tiles))
	(rover (agent:new-rover :info *info*
				:clock clock
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
    (clock-set clock 1)
    (agent:dostep *agent*)
    (is (equalp (command-state move-cmd) :done))
    (is (equalp (agent:location rover) '(0 1)))

    (agent:stop rover)))

(run! 'agent-tests)
