;; Agents act in the universe based on the admin's commands
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
	   #:submit-command
	   #:agent-step))

(in-package :alaman.agent)

(defun dbg (agent &rest args)
  (format t "agent ~a: ~a"
	  (core:agent-name (info agent))
	  (apply #'format nil args)))

;; TODO: implement mockable clock
(defun new-clock () nil)
(defun clock-time (clock)
  (get-universal-time))

(defclass agent ()
  ((info
    :initarg :info
    :accessor info)
   (root
    :initarg :root)
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

(defun new-agent (&key info root ns clock)
  "Create a new agent. info: core:agent"
  (make-instance 'agent
		 :info info
		 :root root
		 :ns ns
		 :clock clock))

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
    (:sleeping (step-sleeping agent))
    (t (error "unrecognized state"))))

(defmethod step-active (agent)
  (exec-commands agent))

(defmethod exec-commands (agent)
  (printf (commands agent)))

(defmethod step-sleeping (agent)
  (let ((ts (clock-time (clock agent))))
    (when (> ts (sleep-until agent))
      (wakeup agent))))

(defmethod wakeup (agent)
  (dbg agent "waking up")
  (set-state agent :active)
  (setf (sleep-until agent) nil))

(defmethod submit-command (agent command)
  (push command (commands agent)))


(defvar *ns* (ns:init))
(defvar *agent* (new-agent :info (alaman.core:make-agent)
			   :ns *ns*
			   :clock (new-clock)))
(start *agent*)
(set-state *agent* :active)
(print (info *agent*))
(submit-command *agent* "sleep")
(set-state *agent* :sleeping)
(setf (sleep-until *agent*) 0)
(step-agent *agent*)
