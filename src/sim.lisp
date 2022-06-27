(defpackage alaman.sim
  (:use #:cl)
  (:import-from :alaman.admin)
  (:import-from :alaman.agent)
  (:import-from :alaman.core)
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
  (admin nil)
  (agents nil))

;; TODO: Init universe
(defun rand-spec ()
  (let* ((nameserver (ns:init))
	(clock (time:new-system-clock)))
    (core:make-spec
     :clock clock
     :nameserver nameserver
     :admin (admin:init :folder nil
			:ns nameserver
			:clock clock))))

(defun init (spec))
(defun start (sim) nil)
(defun run (sim) nil)
(defun dostep (sim) nil)
(defun stop (sim) nil)
(defun spawn-agent (sim &optional agent) "Spawn an agent" nil)
(defun submit (command) nil)
(defun exec (command) nil)
(defun pprint-sim (sim) nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)
