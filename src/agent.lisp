(defpackage alaman.agent
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :alaman.ns)
  (:import-from :alaman.core)
  (:local-nicknames
   (:core :alaman.core)
   (:ns :alaman.ns))
  (:export #:make-agent-name
	   #:make-agent-names
	   #:init
	   #:info
	   #:state
	   #:start
	   #:stop
	   #:submit
	   #:dostep))

(in-package :alaman.agent)

;; Names ------------------------------------------------------------

(defvar *agent-name-prefixes* '(aerobic magic miserly misfit insolent tall short speedy zippy turvy rotund))
(defvar *agent-name-suffixes* '(agent automaton bot droid machine))

(defun make-agent-name ()
  (format nil "~a-~a"
	  (rand-select *agent-name-prefixes*)
	  (rand-select *agent-name-suffixes*)))

(defun make-agent-name-with-suffix ()
  (format nil "~a-~a" (make-agent-name) (random 10000)))

(defun try-make-unique-agent-name (used)
  (dotimes (i 10)
    (let ((name (if (> i 5) (make-agent-name-with-suffix) (make-agent-name))))
      (when (null (find name used))
	(return name)))))

(defun make-unique-agent-name (used)
  (let ((name (try-make-unique-agent-name used)))
    (if (null name)
	(error "could not make unique agent name")
	name)))

(defun make-agent-names (count)
  (let ((names nil))
    (dotimes (i count)
      (setf names(cons (make-unique-agent-name names) names)))
    names))

(defclass agent ()
  ((info
    :initarg :info)
   (ns
    :initarg :ns
    :reader nameserv)
   (clock
    :initarg :clock
    :reader clock)
   (commands
    :initform nil
    :accessor commands)
   (sleep-until
    :initform nil
    :accessor sleep-until)))

(defun dbg (agent &rest args)
  (format t "agent ~a: ~a~%"
	  (core:agent-info-name (pinfo agent))
	  (apply #'format nil args)))

(defun init (&key info ns clock)
  "Create a new agent."
  (make-instance 'agent
		 :info info
		 :ns ns
		 :clock clock))

(defmethod info (agent)
  "Returns a copy of the agent's core:agent-info."
  (core:copy-agent-info (slot-value agent 'info)))

(defmethod pinfo (agent)
  "Internal version of info without the copy."
  (slot-value agent 'info))

(defmethod start (agent)
  "Start and return the agent. Must be called after creation."
  (dbg agent "start")
  (set-state agent :active)
  (register agent)
  agent)

(defmethod stop (agent)
  "Stop and return the agent."
  (dbg agent "stop")
  (set-state agent :stopped)
  (register agent)
  agent)

(defmethod ns-entry-name (agent)
  (format nil "/agent/~a" (core:agent-info-name (pinfo agent))))

(defmethod state (agent)
  (core:agent-info-state (pinfo agent)))

(defmethod register (agent)
  (dbg agent "registering with nameserver")
  (ns:register
   (nameserv agent)
   (ns-entry-name agent)
   agent
   (info agent)))

(defmethod set-state (agent state)
  (dbg agent "set-state ~a" state)
  (setf (core:agent-info-state (pinfo agent)) state)
  agent)

(defmethod submit (agent command)
  "Submit a command for execution."
  (push command (commands agent)))

(defmethod dostep (agent)
  "Advance the agent to the current clock time. Returns a list of completed commands."
  (dbg agent "step")
  (case (state agent)
    (:active (step-active agent))
    (:stopped nil)  ;; No-op
    (:sleeping (step-sleeping agent))
    (t (error "unrecognized state")))
  agent)

(defmethod step-active (agent)
  (exec-commands agent))

;; TODO: implement
(defmethod exec-commands (agent)
  nil)

(defmethod step-sleeping (agent)
  (let ((ts (clock-time (clock agent))))
    (when (> ts (sleep-until agent))
      (wakeup agent))))

(defmethod wakeup (agent)
  (dbg agent "waking up")
  (set-state agent :active)
  (setf (sleep-until agent) nil))
