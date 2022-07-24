(defpackage alaman.admin
  (:use #:cl)
  (:import-from #:alaman.agent)
  (:import-from #:alaman.core)
  (:import-from #:alaman.ns)
  (:import-from #:alexandria :hash-table-values :random-elt)
  (:local-nicknames
   (:agent :alaman.agent)
   (:core :alaman.core)
   (:ns :alaman.ns))
  (:export #:init
	   #:start
	   #:run-step
	   #:stop
	   #:submit
	   #:list-agents))
(in-package :alaman.admin)

(defstruct admin
  "Manages agents and provides admin functionality."
  (folder "")
  (ns nil)
  (clock nil)
  (agents (make-hash-table :test #'equal))
  (devices (make-hash-table :test #'equal))
  (commands nil)
  (events nil))

(defun dbg (admin &rest args)
  (format t "admin: ~a~%" (apply #'format nil args)))

(defun init (&key folder ns clock)
  (make-admin :folder folder :ns ns :clock clock))

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

(defun run-step (admin)
  nil)

(defun stop (admin)
  nil)

(defun register-agent (admin info)
  nil)

(defun describe-agent (admin name)
  nil)

(defun list-agents (admin)
  (hash-table-values (admin-agents admin)))

(defun submit (admin command)
  (let* ((agent-id (core:cmd-agent-id command))
	 (agent (gethash agent-id (admin-agents admin))))
    (if agent-id
	(agent:submit agent command)
	(plan admin command))))

(defun plan (admin command)
  (agent:submit (random-elt (hash-table-values (admin-agents admin)))
		command))

(defun cancel (command-id)
  nil)

(defun list-commands ()
  nil)

(defun agent-check-in (agent-id)
  nil)

(defun agent-log-events (agent-id events)
  nil)
