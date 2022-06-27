;; Core types
(defpackage alaman.core
  (:use #:cl)
  (:nicknames "core")
  (:export #:agent-info
   	   #:clock-time
	   #:command
	   #:device-info
	   #:event
	   #:new-system-clock
	   #:object
	   #:spec
	   #:tile
	   #:universe))
(in-package :alaman.core)

(defstruct spec
  "Simulation specification."
  (name nil)
  (folder nil)
  (universe nil)
  (admin nil)
  (agents nil)
  (nameserver nil)
  (clock nil))

(defstruct universe
  "A map of the universe."
  (dims '(1000 1000))
  (tiles nil)
  (entities nil))

(defstruct tile
  "A tile in the universe."
  (location nil)
  (entities nil))

(defstruct object
  "An object in the universe."
  (id "")
  (name "")
  (kind nil)
  (tags nil)
  (vars nil)
  (version 0)
  (created-at nil)
  (updated-at nil))

(defstruct agent-info
  "Information about an agent."
  (id "")
  (name "")
  (address "")
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil)
  (location nil)
  (last-seen nil))

(defstruct device-info
  "Information about a device."
  (id "")
  (name "")
  (kind nil)
  (agent-id nil)
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil))

(defstruct command
  "A command to be performed by an agent."
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
  "An event recorded by an agent."
  (id "")
  (kind nil)
  (tags nil)
  (vars nil)
  (timestamp nil))

;; TODO: Support different implementations
(defun new-system-clock () nil)
(defun clock-time (clock)
  (get-universal-time))

;;; TODO: Limit exports?
(defun export-all (pkg)
  (let ((pack (find-package pkg)))
    (do-symbols (sym pack)
      (when (eql (symbol-package sym) pack) (export sym)))))
(export-all :alaman.core)
