(defpackage alaman.admin
  (:use #:cl)
  (:import-from #:alaman.core)
  (:import-from #:alaman.ns)
  (:local-nicknames (:core :alaman.core)
		    (:ns :alaman.ns))
  (:export #:new-admin
	   #:start
	   #:stop))

(in-package :alaman.admin)

(defstruct admin
  "Manages agents and provides admin functionality."
  (root "")
  (universe nil)
  (nameserv nil)
  (clock nil)
  (agents (make-hash-table :test #'equal))
  (devices (make-hash-table :test #'equal))
  (command-queue nil)
  (events nil))

(defun new-admin (&key root universe ns clock)
  (make-admin :root root :universe universe :nameserv ns :clock clock))

(defun start (admin)
  (register admin)
  (fetch-agents admin))

(defun register (admin)
  (ns:register (admin-nameserv admin) "/admin" admin ""))

(defun fetch-agents (admin)
  (let ((entries (ns:children (admin-nameserv admin) "/agent")))
    (dolist (entry entries)
      (print entry)
      (let* ((info (nth 2 entry))
	     (id (core:agent-id info))
	     (agents (admin-agents admin))))
      (setf (gethash id agents) info))))

(defun stop ()
  nil)

(defun register-agent (info)
  nil)

(defun describe-agent (name)
  nil)

(defun list-agents ()
  nil)

(defun send-command (command)
  nil)

(defun list-commands ()
  nil)

(defun cancel-command (id)
  nil)

(defun agent-check-in (agent-id)
  nil)

(defun agent-log-events (agent-id events)
  nil)

