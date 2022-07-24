(defpackage alaman.sim
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:import-from :alaman.time)
  (:import-from :spinneret)
  (:local-nicknames
   (:admin :alaman.admin)
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:am :alaman.map)
   (:time :alaman.time)
   (:sp :spinneret))
  (:export #:make-spec
           #:make-sim
	   #:init
	   #:start
	   #:run
	   #:run-step
	   #:submit
	   #:stop
	   #:save
	   #:load-from))
(in-package :alaman.sim)

(defstruct spec
  "Simulation specification."
  (name "")
  (folder nil)
  (seed nil)
  (dims '(100 100))
  (min-agents 1)
  (max-agents 3)
  (clock-speed 1))

(defstruct sim
  "Simulation state."
  (id nil)
  (spec nil)
  (universe nil)
  (clock nil)
  (nameserver nil)
  (admin nil)
  (agents nil))

(defun init (&optional (spec (core:make-spec))
	     &key (clock (new-system-clock))
	       (nameserver (ns:init))
	       (universe (create-universe spec))
	       (agents (create-agents spec clock nameserver))
	       (admin (admin:init :folder nil :ns nameserver :clock clock)))
  (make-sim
   :id (new-id)
   :spec spec
   :clock clock
   :nameserver nameserver
   :universe universe
   :admin admin
   :agents (or agents )))

(defun create-universe (spec)
  (make-universe :dims (core:spec-dims spec)
		 :tiles (am:uniform-map (core:spec-dims spec))))

(defun create-agents (spec clock nameserver)
  (let* ((count (core:rand-range-incl (spec-min-agents spec)
				      (spec-max-agents spec)))
	 (names (agent:make-agent-names count)))
    (assert (>= count 0))
    (loop for name in names
	  collect (agent:init
		   :info (make-agent-info :id (new-id) :name name)
		   :clock clock
		   :ns nameserver))))

(defun start (sim)
  (dolist (agent (sim-agents sim))
    (agent:start agent))
  (admin:start (sim-admin sim)) ; admin last for ns registration
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
  (admin:submit (sim-admin sim) command))


(defun spawn-agent (sim &optional agent) "Spawn an agent" nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)

(defun tiles (sim)
  (core:universe-tiles (sim-universe sim)))

(defvar *html-style* "
.tile { width: 20px; height: 20px; }
.wheat { background: yellow; }
.grass { background: green; }
.rock { background: grey; }
.water { background: blue; }
")

(defun render-html (sim)
  (sp:with-html
    (:html
     (:head
      (:title "alaman")
      (:style *html-style*))
     (:body
      (:h1 "alaman")
      (:h2 "map")
      (am:render-html (tiles sim))
      (:h2 "admin")
      (:h2 "agents")))))

(defun render-html-string (sim)
  (let ((sp:*html-style* :human))
    (sp:with-html-string (render-html sim))))
