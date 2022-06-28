(defpackage alaman.sim
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.ns)
  (:import-from :alaman.time)
  (:local-nicknames
   (:admin :alaman.admin)
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:time :alaman.time))
  (:export #:rand-spec
	   #:init
	   #:start
	   #:run
	   #:dostep
	   #:stop
	   #:save
	   #:load-from))
(in-package :alaman.sim)

(defstruct sim
  "Simulation state."
  (id "")
  (spec nil)
  (universe nil)
  (clock nil)
  (admin nil)
  (agents nil))

;; TODO: Init universe
(defun rand-spec ()
  (let* ((nameserver (ns:init))
	(clock (time:new-system-clock)))
    (make-spec
     :clock clock
     :nameserver nameserver
     :admin (admin:init :folder nil
			:ns nameserver
			:clock clock))))

(defun init (spec)
  (make-sim :id (new-id)
	    :spec spec
	    :universe (spec-universe spec)
	    :clock (spec-clock spec)
	    :admin (spec-admin spec)
	    :agents (spec-agents spec)))

(defun start (sim)
  (admin:start (sim-admin sim))
  (dolist (agent (sim-agents sim))
    (agent:start agent))
  sim)

(defun dostep (sim)
  (let ((clock (sim-clock sim)))
    (clock-tick clock)
    (clock-pin clock)
    (admin:dostep (sim-admin sim))
    (dolist (agent (sim-agents sim))
      (agent:dostep (agent)))
    (clock-unpin clock))
  sim)

(defun stop (sim)
  (admin:stop (sim-admin sim))
  (dolist (agent (sim-agents sim))
    (agent:stop (agent)))
  sim)

(defun spawn-agent (sim &optional agent) "Spawn an agent" nil)
(defun submit (command) nil)
(defun exec (command) nil)
(defun pprint-sim (sim) nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)
