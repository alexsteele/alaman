(defpackage alaman.agent
  (:use #:cl #:alaman.core #:alaman.time)
  (:import-from :priority-queue)
  (:import-from :alaman.ns)
  (:import-from :alaman.core)
  (:import-from :alaman.command)
  (:import-from :alaman.event)
  (:local-nicknames
   (:core :alaman.core)
   (:cmd :alaman.command)
   (:event :alaman.event)
   (:ns :alaman.ns)
   (:pq :priority-queue))
  (:export #:make-agent-name
	   #:make-agent-names
	   #:init
	   #:info
	   #:state
	   #:location
	   #:start
	   #:stop
	   #:submit
	   #:dostep
	   ;; Blueprints
	   #:new-rover))

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

;; Agent ------------------------------------------------------------

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
   (devices
    :initform nil
    :accessor devices)
   (events
    :initform nil
    :accessor events)
   (actions
    :initform (pq:make-pqueue #'equalp)
    :accessor actions)
   (next-actions
    :initform (pq:make-pqueue #'equalp)
    :accessor next-actions)
   (step-start
    :initform nil
    :accessor step-start)
   (step-end
    :initform nil
    :accessor step-end)))

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

(defmethod state (agent)
  (core:agent-info-state (pinfo agent)))

(defmethod location (agent)
  (core:agent-info-location (pinfo agent)))

(defmethod start (agent)
  "Start and return the agent. Must be called after creation."
  (dbg agent "start")
  (set-state agent :active)
  (register agent)
  (setf (step-end agent) (agent-time agent))
  agent)

(defmethod stop (agent)
  "Stop and return the agent."
  (dbg agent "stop")
  (set-state agent :stopped)
  (register agent)
  agent)

;; TODO: Record command. Defer planning to next step execution for prioritization.
(defmethod submit (agent command)
  "Submit a command for execution."
  (plan agent command))

;; TODO: Return completed commands.
(defmethod dostep (agent)
  "Advance the agent to the current clock time. Returns a list of completed commands."
  (dbg agent "step")
  (case (state agent)
    (:active (one-step agent))
    (:sleeping (one-step agent))
    (:stopped nil)  ;; No-op
    (t (error (format nil "unrecognized state ~a" (state agent)))))
  agent)

;; Internal  -------------------------------------------------------------------

(defmethod register (agent)
  (dbg agent "registering with nameserver")
  (ns:register
   (nameserv agent)
   (ns-entry-name agent)
   agent
   (info agent)))

(defmethod ns-entry-name (agent)
  (format nil "/agent/~a" (core:agent-info-name (pinfo agent))))

(defmethod set-state (agent state)
  (dbg agent "set-state ~a" state)
  (setf (core:agent-info-state (pinfo agent)) state)
  agent)

(defmethod agent-time (agent)
  (clock-time (clock agent)))

(defmethod one-step (agent)
  (start-step agent)
  (run-step agent))

(defmethod start-step (agent)
  (setf (step-start agent) (step-end agent))
  (setf (step-end agent) (agent-time agent))
  (rotate-actions agent))

(defmethod rotate-actions (agent)
  (setf (actions agent) (next-actions agent))
  (setf (next-actions agent) (pq:make-pqueue #'equalp)))

;; TODO: Only run actions that can execute within the time slice
(defmethod run-step (agent)
  (loop while (any-actions agent)
	do (exec-action agent)))

(defstruct action
  (fn nil)
  (scope nil)
  (description "")
  (device nil)
  (priority 99))

(defmethod push-action (agent action)
  (pq:pqueue-push action (action-priority action) (next-actions agent)))

(defmethod any-actions (agent)
  (not (pq:pqueue-empty-p (actions agent))))

(defmethod exec-action (agent)
  (let ((action (pq:pqueue-pop (actions agent))))
    (funcall (action-fn action))))

(defmethod record (agent event)
  (push event (events agent)))

(defun finish-command (agent cmd)
  (setf (command-state cmd) :done)
  (setf (command-end-time cmd) (agent-time agent))
  (record agent (done-event cmd)))

;; Planning  -------------------------------------------------------------------

(defmethod plan (agent cmd)
  (case (command-kind cmd)
    (:no-op (plan-no-op agent cmd))
    (:sleep (plan-sleep agent cmd))
    (otherwise (error (format nil "unrecognized command kind ~a" command-kind)))))

(defmethod plan-no-op (agent cmd)
  (push-action agent (make-action :fn #'(lambda () (exec-no-op agent cmd))
				  :scope :global
				  :description :no-op)))

(defmethod plan-sleep (agent cmd)
  (push-action agent (make-action :fn #'(lambda () (exec-sleep agent cmd))
				  :scope :global
				  :description :sleep)))

;; Actions  -------------------------------------------------------------------

(defun exec-no-op (agent cmd)
  (dbg agent "exec-no-op")
  (finish-command agent cmd))

(defun exec-sleep (agent cmd)
  (dbg agent "exec-sleep ~a" cmd)
  (let* ((sleep-until (cmd:param cmd :until))
	 (start (step-start agent))
	 (deadline (step-end agent))
	 (can-finish (<= sleep-until deadline))
	 (end (min deadline sleep-until)))
    (if can-finish
	(progn
	  (set-state agent :active)
	  (finish-command agent cmd))
	(progn
	  (set-state agent :sleeping)
	  (setf (command-state cmd) :running)
	  (push-action agent (make-action :fn #'(lambda () (exec-sleep agent cmd))
					  :scope :global
					  :description :sleep
					  :priority 0))))))

;; Agent Blueprints ------------------------------------------------------------

;; TODO: Implement
(defun new-rover (&key info clock ns universe location max-speed)
  nil)
