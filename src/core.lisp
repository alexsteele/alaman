;; Core types
(defpackage alaman.core
  (:use #:cl)
  (:nicknames "core")
  (:export #:agent-info
	   #:device-info
	   #:command
	   #:event
	   #:object
	   #:universe
	   #:new-system-clock
	   #:clock-time))
(in-package :alaman.core)

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
  "An event recorded by an agent."
  (id "")
  (kind nil)
  (tags nil)
  (vars nil)
  (timestamp nil))

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

(defstruct universe
  "A map of the universe."
  (dims '(1000 1000))
  (entities nil))

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
