(defpackage alaman.sim
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.ns)
  (:import-from :alaman.world)
  (:import-from :alaman.time)
  (:import-from :spinneret)
  (:local-nicknames
   (:admin :alaman.admin)
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:world :alaman.world)
   (:time :alaman.time)
   (:sp :spinneret))
  (:export #:make-config
           #:make-sim
	   #:init
	   #:start
	   #:run
	   #:run-step
	   #:submit
	   #:stop
	   #:save
	   #:load-from
	   #:render-html
	   #:render-html-string))
(in-package :alaman.sim)

(defstruct config
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
  (config nil)
  (world nil)
  (clock nil)
  (nameserver nil)
  (admin nil)
  (agents nil))

(defun init (&optional (config (make-config))
	     &key (clock (new-system-clock))
	       (nameserver (ns:init))
	       (world (create-world config))
	       (agents (create-agents config world clock nameserver))
	       (admin (admin:init :folder nil :ns nameserver :clock clock)))
  (make-sim
   :id (new-id)
   :config config
   :clock clock
   :nameserver nameserver
   :world world
   :admin admin
   :agents agents))

(defun create-world (config)
  (make-world :dims (config-dims config)
	      :tiles (world:uniform-map (config-dims config))))

(defun create-agents (config world clock nameserver)
  (let* ((count (core:rand-range-incl (config-min-agents config)
				      (config-max-agents config)))
	 (names (agent:make-agent-names count)))
    (assert (>= count 0))
    (loop for name in names
	  collect (agent:new-rover
		   :info (make-agent-info :id (new-id)
					  :name name
					  :location (core:rand-point (config-dims config)))
		   :world world
		   :clock clock
		   :ns nameserver
		   :world world))))

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
    (world:run-step (tiles sim) 1)
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
  (core:world-tiles (sim-world sim)))

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
      (world:render-html (tiles sim))
      (:h2 "admin")
      (:h2 "agents")
      (render-agents-html sim)))))

(defun render-html-string (sim)
  (let ((sp:*html-style* :human))
    (sp:with-html-string (render-html sim))))

(defun render-agents-html (sim)
  (sp:with-html
   (dolist (agent (sim-agents sim))
     (:div
      (:h3 (core:agent-info-name (agent:info agent)))
      (:p (format nil "~a" (agent:info agent)))))))
