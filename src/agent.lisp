(defpackage alaman.agent
  (:use #:cl #:alaman.core)
  (:import-from :alaman.ns)
  (:import-from :alaman.core)
  (:local-nicknames
   (:core :alaman.core)
   (:ns :alaman.ns))
  (:export #:new-agent
	   #:start
	   #:stop
	   #:submit-command
	   #:agent-step))

(in-package :alaman.agent)

(defclass agent ()
  ((info
    :initarg :info
    :accessor info)
   (ns
    :initarg :ns
    :accessor nameserv)
   (clock
    :initarg :clock
    :accessor clock)
   (commands
    :initform nil
    :accessor commands)
   (sleep-until
    :initform nil
    :accessor sleep-until)))

(defun dbg (agent &rest args)
  (format t "agent ~a: ~a"
	  (core:agent-info-name (info agent))
	  (apply #'format nil args)))

(defun new-agent (&key info ns clock)
  "Create a new agent."
  (make-instance 'agent
		 :info info
		 :ns ns
		 :clock clock))

(defmethod start (agent)
  "Start the agent. Must be called first after creation. Returns the agent."
  (register agent)
  agent)

(defmethod ns-entry-name (agent)
  (format nil "/agent/~a" (core:agent-info-name (info agent))))

(defmethod register (agent)
  (dbg agent "registering with nameserver")
  (ns:register
   (nameserv agent)
   (ns-entry-name agent)
   agent
   (info agent)))

(defmethod set-state (agent state)
  (dbg agent "set-state ~a" state)
  (setf (core:agent-state (info agent)) state))

(defmethod stop (agent)
  nil)

(defmethod dostep (agent)
  "Advance the agent to the current clock time."
  (case (core:agent-state (info agent))
    (:active (step-active agent))
    (:sleeping (step-sleeping agent))
    (t (error "unrecognized state"))))

(defmethod step-active (agent)
  (exec-commands agent))

(defmethod exec-commands (agent)
  (printf (commands agent)))

(defmethod step-sleeping (agent)
  (let ((ts (core:clock-time (clock agent))))
    (when (> ts (sleep-until agent))
      (wakeup agent))))

(defmethod wakeup (agent)
  (dbg agent "waking up")
  (set-state agent :active)
  (setf (sleep-until agent) nil))

(defmethod submit-command (agent command)
  (push command (commands agent)))
