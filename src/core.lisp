(defpackage alaman.core
  (:use #:cl)
  (:export #:agent
	   #:device
	   #:command
	   #:event
	   #:feature
	   #:universe))
(in-package :alaman.core)

(defstruct agent
  "An agent in the universe. Agents execute commands and log events."
  (id "")
  (name "")
  (address "")
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil)
  (location nil)
  (last-seen nil))

(defstruct device
  "A device. Devices may be passive (e.g. thermometer) or
active (e.g. actuator). Devices record data in 'vars' and may log
events. Active devices can execute commands. Devices may be attached
to agents."
  (id "")
  (name "")
  (kind nil)
  (agent-id nil)
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil))

(defstruct command
  "A command is an action to be performed by an agent or device."
  (id "")
  (agent-id "")
  (kind nil)
  (state nil)
  (params nil)
  (run-after nil)
  (start-time nil)
  (end-time nil)
  (result nil))

(defstruct event
  (id "")
  (kind nil)
  (tags nil)
  (vars nil)
  (timestamp nil))

(defstruct feature
  "A feature is an inanimate entity in the universe."
  (id "")
  (name "")
  (kind nil)
  (tags nil)
  (vars nil)
  (version 0)
  (created-at nil)
  (updated-at nil))

(defstruct universe
  "A map of the universe."
  (dims '(1000 1000))
  (entities nil))

;;; TODO: Limit exports?
(defun export-all (pkg)
  (let ((pack (find-package pkg)))
    (do-symbols (sym pack)
      (when (eql (symbol-package sym) pack) (export sym)))))
(export-all :alaman.core)
