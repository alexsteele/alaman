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
  (:export #:rand-spec
	   #:init
	   #:start
	   #:run
	   #:dostep
	   #:stop
	   #:save
	   #:load-from))
(in-package :alaman.sim)

(defun make-agents (count clock nameserver)
  (let ((names (agent:make-agent-names count)))
    (loop for name in names
	  collect (agent:init :info (make-agent-info :id (new-id) :name name)
			      :clock clock
			      :ns nameserver))))

(defun new-universe ()
  (let ((dims '(100 100)))
    (make-universe :dims dims
		   :tiles (am:uniform-map dims))))

(defun rand-spec ()
  "Creates a randomized core:spec."
  (let* ((nameserver (ns:init))
	 (clock (new-system-clock)))
    (make-spec
     :clock clock
     :nameserver nameserver
     :universe (new-universe)
     :admin (admin:init :folder nil
			:ns nameserver
			:clock clock)
     :agents (make-agents 3 clock nameserver))))

(defstruct sim
  "Simulation state."
  (id "")
  (spec nil)
  (universe nil)
  (clock nil)
  (admin nil)
  (agents nil))

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
    (format t "AAA ~a" clock)
    (clock-tick clock)
    (clock-pin clock)
    (admin:dostep (sim-admin sim))
    (dolist (agent (sim-agents sim))
      (agent:dostep agent))
    (clock-unpin clock))
  sim)

(defun stop (sim)
  (admin:stop (sim-admin sim))
  (dolist (agent (sim-agents sim))
    (agent:stop agent))
  sim)

(defun spawn-agent (sim &optional agent) "Spawn an agent" nil)
(defun submit (command) nil)
(defun exec (command) nil)
(defun pprint-sim (sim) nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)
