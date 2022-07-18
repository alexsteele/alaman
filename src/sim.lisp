(defpackage alaman.sim
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:import-from :alaman.time)
  (:local-nicknames
   (:admin :alaman.admin)
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:am :alaman.map)
   (:time :alaman.time))
  (:export #:make-sim
	   #:init
	   #:start
	   #:run
	   #:run-step
	   #:stop
	   #:save
	   #:load-from))
(in-package :alaman.sim)

(defstruct sim
  "Simulation state."
  (id nil)
  (spec nil)
  (universe nil)
  (clock nil)
  (nameserver nil)
  (admin nil)
  (agents nil))

(defun init (spec)
  (let* ((clock (new-system-clock))
	 (nameserver (ns:init))
	 (num-agents (core:rand-range-incl (spec-min-agents spec)
					   (spec-max-agents spec))))
    (make-sim
     :id (new-id)
     :spec spec
     :clock clock
     :nameserver nameserver
     :universe (create-universe)
     :admin (admin:init :folder nil :ns nameserver :clock clock)
     :agents (create-agents num-agents clock nameserver))))

(defun create-universe ()
  (let ((dims '(100 100)))
    (make-universe :dims dims :tiles (am:uniform-map dims))))

(defun create-agents (count clock nameserver)
  (let ((names (agent:make-agent-names count)))
    (loop for name in names
	  collect (agent:init
		   :info (make-agent-info :id (new-id) :name name)
		   :clock clock
		   :ns nameserver))))

(defun start (sim)
  (admin:start (sim-admin sim))
  (dolist (agent (sim-agents sim))
    (agent:start agent))
  sim)

(defun run-step (sim)
  (print "sim: run-step")
  (let ((clock (sim-clock sim)))
    (clock-tick clock)
    (clock-pin clock)
    (admin:run-step (sim-admin sim))
    (dolist (agent (sim-agents sim))
      (agent:run-step agent))
    (clock-unpin clock))
  sim)

(defun stop (sim)
  (admin:stop (sim-admin sim))
  (dolist (agent (sim-agents sim))
    (agent:stop agent))
  sim)

(defun submit (sim command)
  (admin:submit command))

(defun spawn-agent (sim &optional agent) "Spawn an agent" nil)
(defun exec (sim command) nil)
(defun pprint-sim (sim) nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)
(defun render-html (sim) nil)
