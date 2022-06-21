(defpackage alaman.agent
  (:use #:cl)
  (:import-from :alaman.ns)
  (:import-from :alaman.core)
  (:local-nicknames
   (:core :alaman.core)
   (:ns :alaman.ns))
  (:export #:new-agent
	   #:start
	   #:stop
	   #:agent-step))

(in-package :alaman.agent)

(defun dbg (agent &rest args)
  (format t "agent ~a: ~a"
	  (core:agent-name (info agent))
	  (apply #'format nil args)))

(defclass agent ()
  ((info
    :initarg :info
    :accessor info)
   (root
    :initarg :root)
   (ns
    :initarg :ns
    :accessor nameserv)
   (commands
    :initform nil
    :accessor commands)))

(defun new-agent (&key info root ns)
  "Create a new agent. info: core:agent"
  (make-instance 'agent
		 :info info
		 :root root
		 :ns ns))

(defmethod register (agent)
  (dbg agent "registering ~a" (core:agent-name (info agent)))
  (ns:register
   (nameserv agent)
   (format nil "/agent/~a" (core:agent-name (info agent)))
   agent
   (info agent)))

(defmethod start (agent)
  (register agent))

(defmethod set-state (agent state)
  (dbg agent "set-state ~a" state)
  (setf (core:agent-state (info agent)) state))

(defmethod stop (agent)
  nil)

;;; TODO: rename. avoid collision with cl:step
(defmethod step-agent (agent)
  (case (core:agent-state (info agent))
	(:active (step-active agent))
	(t (error "unrecognized state"))))

(defmethod step-active (agent)
  (exec-commands agent))

(defmethod exec-commands (agent)
  (printf (commands agent)))

(defvar *ns* (ns:init))
(defvar *agent* (new-agent :info (alaman.core:make-agent) :ns *ns*))
(start *agent*)
(set-state *agent* :active)
(print (info *agent*))
