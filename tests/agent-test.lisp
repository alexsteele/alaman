(defpackage alaman/tests/agent
  (:use #:alaman.time)
  (:import-from :alaman.agent)
  (:import-from :alaman.core)
  (:import-from :alaman.command)
  (:import-from :alaman.device)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:local-nicknames
   (:agent :alaman.agent)
   (:ns :alaman.ns)
   (:core :alaman.core)
   (:cmd :alaman.command)
   (:dev :alaman.device)
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

(test run-step-no-commands
  (let* ((info (make-agent-info :id "agent-id" :name "name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock)))
    (agent:start A)
    (clock-set clock 10)
    (agent:run-step A)
    (agent:stop A)))

;; command tests

(test no-op-command
  (let* ((info (make-agent-info :id "agent-id" :name "test-name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock))
	 (no-op (cmd:no-op)))
    (agent:start A)
    (agent:submit A no-op)
    (clock-set clock 10)
    (agent:run-step A)

    (is (equalp :done (cmd:state no-op)))

    (agent:stop A)))

(test sleep-command
  (let* ((info (make-agent-info :id "agent-id" :name "test-name"))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (A (agent:init :info info :ns NS :clock clock))
	 (sleep-cmd (cmd:sleep-until 20)))

    ;; Submit at T=0
    (agent:start A)
    (agent:submit A sleep-cmd)

    ;; Step to T=10
    (clock-set clock 10)
    (agent:run-step A)
    (is (equalp :running (core:command-state sleep-cmd)))
    (is (equalp :sleeping (agent:state A)))

    ;; Step to T=20. Finish.
    (clock-set clock 20)
    (agent:run-step A)
    (is (equalp :done (core:command-state sleep-cmd)))
    (is (equalp :active (agent:state A)))

    (agent:stop A)))

(test move-command
  (let* ((info (make-agent-info :id "agent-id" :name "test-name" :location '(0 0)))
	 (clock (new-fixed-clock :init-val 0 :tick-amount 0))
	 (NS (ns:init))
	 (tiles (am:uniform-map '(10 10) :kind :grass :climate :sunny))
	 (U (make-universe :tiles tiles))
	 (bat (dev:new-battery))
	 (eng (dev:new-engine :battery bat))
	 (devices (list bat eng))
	 (A (agent:init :info info :ns NS :clock clock :devices devices))
	 (move-cmd (cmd:move-to '(0 9))))

    (agent:start A)
    (agent:submit A move-cmd)

    (clock-set clock 5)
    (agent:run-step A)
    (is (equalp :running (core:command-state move-cmd)))
    (is (equalp '(0 5) (agent:location A)))

    (clock-set clock 9)
    (agent:run-step A)
    (is (equalp :done (core:command-state move-cmd)))
    (is (equalp '(0 9) (agent:location A)))

    (agent:stop A)))

(run! 'agent-tests)
