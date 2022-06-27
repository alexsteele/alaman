(defpackage alaman.admin
  (:use #:cl)
  (:import-from #:alaman.core)
  (:import-from #:alaman.ns)
  (:import-from #:alexandria :hash-table-values)
  (:local-nicknames (:core :alaman.core)
		    (:ns :alaman.ns))
  (:export #:init
	   #:start
	   #:dostep
	   #:stop
	   #:list-agents))
(in-package :alaman.admin)

(defstruct admin
  "Manages agents and provides admin functionality."
  (root "")
  (ns nil)
  (clock nil)
  (agents (make-hash-table :test #'equal))
  (devices (make-hash-table :test #'equal))
  (command-queue nil)
  (events nil))

(defun dbg (admin &rest args)
  (format t "admin: ~a~%" (apply #'format nil args)))

(defun init (&key root ns clock)
  (make-admin :root root :ns ns :clock clock))

(defun start (admin)
  (dbg admin "start")
  (register admin)
  (fetch-agent-info admin)
  admin)

(defun register (admin)
  (ns:register (admin-ns admin) "/admin" admin nil))

(defun fetch-agent-info (admin)
  "Fetch the latest agent info from the nameserver."
  (let ((entries (ns:children (admin-ns admin) "/agent")))
    (dolist (entry entries)
      (let* ((info (ns:entry-data entry))
	     (id (core:agent-info-id info))
	     (agents (admin-agents admin)))
	(setf (gethash id agents) info)))))

(defun dostep ()
  nil)

(defun stop ()
  nil)

(defun register-agent (info)
  nil)

(defun describe-agent (name)
  nil)

(defun list-agents (admin)
  (hash-table-values (admin-agents admin)))

(defun submit (command)
  nil)

(defun cancel (command-id)
  nil)

(defun list-commands ()
  nil)

(defun agent-check-in (agent-id)
  nil)

(defun agent-log-events (agent-id events)
  nil)
