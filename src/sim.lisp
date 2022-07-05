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

(defun rand-agent (clock nameserver)
  (let* ((id (new-id))
	 (name (format nil "agent_~a" id))
	 (info (make-agent-info :id id :name name)))
    (agent:init :info info
		:clock clock
		:ns nameserver)))

;; TODO: Init universe
(defun rand-spec ()
  (let* ((nameserver (ns:init))
	 (cl (new-system-clock)))
    (make-spec
     :clock cl
     :nameserver nameserver
     :admin (admin:init :folder nil
			:ns nameserver
			:clock cl)
     :agents (list (rand-agent cl nameserver)
		   (rand-agent cl nameserver)
		   (rand-agent cl nameserver)))))

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

(defvar *spec* (rand-spec))
(defvar *sim* (init *spec*))
(start *sim*)
(format t "~a" *spec*)
(format t "~a" *sim*)
(dostep *sim*)

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
