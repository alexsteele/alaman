(defpackage alaman.agent
  (:use #:cl #:alaman.time)
  (:import-from :priority-queue)
  (:import-from :alaman.ns)
  (:import-from :alaman.core)
  (:import-from :alaman.command)
  (:import-from :alaman.device)
  (:import-from :alaman.event)
  (:local-nicknames
   (:core :alaman.core)
   (:cmd :alaman.command)
   (:dev :alaman.device)
   (:event :alaman.event)
   (:ns :alaman.ns)
   (:pq :priority-queue))
  (:export #:make-agent-name
	   #:make-agent-names
	   #:init
	   #:info
	   #:state
	   #:location
	   #:mass
	   #:start
	   #:stop
	   #:submit
	   #:run-step
	   #:new-rover))

(in-package :alaman.agent)

(defvar *max-agent-speed* 1)

;; Names ------------------------------------------------------------

(defvar *agent-name-prefixes* '(aerobic magic miserly misfit insolent tall short speedy zippy turvy rotund))
(defvar *agent-name-suffixes* '(agent automaton bot droid machine))

(defun make-agent-name ()
  (format nil "~a-~a"
	  (core:rand-select *agent-name-prefixes*)
	  (core:rand-select *agent-name-suffixes*)))

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

;; Agent Blueprints ------------------------------------------------

(defun new-rover (&key info world clock ns)
  (let* ((bag (dev:new-bag))
	 (bat (dev:new-battery))
	 (sp (dev:new-solar-panel :location (core:agent-info-location info)
				  :world world :battery bat))
	 (eng (dev:new-engine :battery bat))
	 (devices (list bag bat sp eng)))
    (init :info info :ns ns :clock clock :devices devices)))

;; Agent ------------------------------------------------------------

(defclass agent ()
  ((info
    :initarg :info
    :reader info)
   (ns
    :initarg :ns
    :reader nameserv)
   (clock
    :initarg :clock
    :reader clock)
   (devices
    :initarg :devices
    :accessor devices)
   (commands
    :initform nil
    :accessor commands)
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
	  (core:agent-info-name (info agent))
	  (apply #'format nil args)))

(defun init (&key info ns clock devices)
  "Create a new agent."
  (make-instance 'agent
		 :info info
		 :ns ns
		 :clock clock
		 :devices devices))

(defmethod state (agent)
  (core:agent-info-state (info agent)))

(defmethod mass (agent)
  (core:agent-info-mass (info agent)))

(defmethod location (agent)
  (core:agent-info-location (info agent)))

(defmethod start (agent)
  "Start and return the agent. Must be called after creation."
  (dbg agent "start")
  (update-mass agent)
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

;; TODO: Defer planning to run-step for better prioritization?
(defmethod submit (agent command)
  "Submit a command for execution."
  (setf (core:cmd-agent-id command) (core:agent-info-id (info agent)))
  (plan agent command))

(defmethod run-step (agent)
  "Advance the agent to the current clock time. Returns a list of completed commands."
  (dbg agent "step")
  (case (state agent)
    (:active (one-step agent))
    (:sleeping (one-step agent)) ;; Continue sleep action
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
   (core:copy-agent-info (info agent))))

(defmethod ns-entry-name (agent)
  (format nil "/agent/~a" (core:agent-info-name (info agent))))

(defmethod set-state (agent state)
  (dbg agent "set-state ~a" state)
  (setf (core:agent-info-state (info agent)) state)
  agent)

(defmethod update-mass (agent)
  (setf (core:agent-info-mass (info agent))
	(reduce '+ (mapcar #'dev:mass (devices agent)))))

(defmethod agent-time (agent)
  (clock-time (clock agent)))

(defmethod one-step (agent)
  (start-step agent)
  (exec-step agent)
  (step-devices agent))

(defmethod start-step (agent)
  (setf (step-start agent) (step-end agent))
  (setf (step-end agent) (agent-time agent))
  (rotate-actions agent))

(defmethod step-duration (agent)
  (- (step-end agent) (step-start agent)))

(defmethod rotate-actions (agent)
  (setf (actions agent) (next-actions agent))
  (setf (next-actions agent) (pq:make-pqueue #'equalp)))

;; TODO: Only run actions that can execute within the time slice
(defmethod exec-step (agent)
  (loop while (any-actions agent)
	do (exec-next-action agent)))

(defmethod step-devices (agent)
  (let ((elapsed-time (step-duration agent)))
    (dolist (device (devices agent))
      (dev:run-step device elapsed-time))))

(defstruct action
  ;; function to execute. no arguments. return elapsed time.
  (fn nil)
  (scope nil)  ;; global | device
  (description nil)
  (device nil)
  (priority 99)) ;; lower goes first

(defmethod push-action (agent action)
  (pq:pqueue-push action (action-priority action) (next-actions agent)))

(defmethod any-actions (agent)
  (not (pq:pqueue-empty-p (actions agent))))

(defmethod exec-next-action (agent)
  (let ((action (pq:pqueue-pop (actions agent))))
    (funcall (action-fn action))))

(defmethod record (agent event)
  (push event (events agent)))

(defun finish-command (agent cmd &key (end-time nil))
  (setf (core:cmd-state cmd) :done)
  (setf (core:cmd-end-time cmd) (or end-time (agent-time agent)))
  (record agent (event:done-event cmd)))

(defun find-dev (agent kind)
  (loop for dev in (devices agent)
	if (equalp (dev:kind dev) kind)
        return dev))

(defun set-location (agent location)
  (setf (core:agent-info-location (info agent)) location)
  (dolist (device (devices agent))
    (setf (dev:location device) location)))

;; Planning  -------------------------------------------------------------------

(defmethod plan (agent cmd)
  (case (core:cmd-kind cmd)
    (:no-op (plan-no-op agent cmd))
    (:sleep (plan-sleep agent cmd))
    (:move (plan-move agent cmd))
    (otherwise (error (format nil "unrecognized command kind ~a" (core:cmd-kind cmd))))))

(defmethod plan-no-op (agent cmd)
  (push-action agent (make-action :fn #'(lambda () (exec-no-op agent cmd))
				  :scope :global
				  :description :no-op)))

(defmethod plan-sleep (agent cmd)
  (push-action agent (make-action :fn #'(lambda () (exec-sleep agent cmd))
				  :scope :global
				  :description :sleep)))

;; TODO: Routing. Check terrain. Check fuel.
(defmethod plan-move (agent cmd)
  (let* ((eng (find-dev agent :engine)))
    (when (not eng)
      (error "no engine"))
    (push-action agent (make-action :fn #'(lambda () (exec-move agent cmd eng))
				    :scope :global
				    :description :move))))

;; -------------------------------------------------------------------
;; Actions
;; -------------------------------------------------------------------
;; Actions are executed by the agent with no arguments. But planning
;; functions may pass arguments inside a closure. Example:
;;
;;  (push-action agent (make-action :fn #'(lambda () (foo arg0 arg1)))
;;
;; Actions should return the time they complete execution. This time
;; must be <= (step-end agent).  To run for more time, an action should
;; schedule another action with push-action.

(defun exec-no-op (agent cmd)
  (dbg agent "exec-no-op")
  (finish-command agent cmd)
  (step-start agent))

(defun exec-sleep (agent cmd)
  (dbg agent "exec-sleep")
  (let* ((sleep-until (cmd:param cmd :until))
	 (deadline (step-end agent))
	 (can-finish (<= sleep-until deadline))
	 (end-time (min deadline sleep-until)))
    (if can-finish
	(progn
	  (set-state agent :active)
	  (finish-command agent cmd))
	(progn
	  (set-state agent :sleeping)
	  (setf (core:cmd-state cmd) :running)
	  (push-action agent (make-action :fn #'(lambda () (exec-sleep agent cmd))
					  :scope :global
					  :description :sleep
					  :priority 0))))
    end-time))

;; ---------- move ----------

(defun point-x (loc) (car loc))
(defun point-y (loc) (car (cdr loc)))
(defun distance (p1 p2)
  (let* ((dx (- (point-x p1) (point-x p2)))
	 (dy (- (point-y p1) (point-y p2))))
    (sqrt (+ (* dx dx) (* dy dy)))))
(defun point-add (p dp)
  (list
   (+ (point-x p) (point-x dp))
   (+ (point-y p) (point-y dp))))

;; TODO: f=ma?
(defun calc-speed (agent eng)
  (declare (ignore agent))
  (* *max-agent-speed* (/ (dev:thrust eng) (dev:max-thrust eng))))

;; Moves `agent` in a straight line towards point `dst` using engine `eng`
;; Returns the end-time of the move
(defun move-step (agent dst eng)
  (let* ((src (location agent))
	 ;; How far do we need to go?
	 (dist (distance src dst))
	 (x-dist (abs (- (point-x src) (point-x dst))))
	 (x-ratio (/ x-dist dist))
	 (y-ratio (- 1 x-ratio))
	 ;; How far can we go this step and how long will it take?
	 (speed (calc-speed agent eng))
	 (step-time (- (step-end agent) (step-start agent)))
	 (step-dist (min dist (* speed step-time)))
	 (step-x (* x-ratio step-dist))
	 (step-y (* y-ratio step-dist))
	 (dp (list step-x step-y))
	 (end-location (point-add src dp))
	 (time-taken (/ step-dist speed))
	 (end-time (+ (step-start agent) time-taken)))
    (dbg agent
	 "move-step: start-location=~a speed=~a end-location=~a destination=~a time-taken=~a"
	 src speed end-location dst time-taken)
    (set-location agent end-location)
    end-time))

(defun exec-move (agent cmd eng)
  (dbg agent "exec-move")
  (let* ((src (location agent))
	 (dst (cmd:param cmd :location))
	 (end-time (step-start agent))
	 (end-location src))

    ;; Move if needed
    (when (not (equalp src dst))
      (setf (core:cmd-state cmd) :running)
      (dev:govern eng 1.0)
      (setf end-time (move-step agent dst eng))
      (setf end-location (location agent)))

    ;; Did we make it?
    (if (equalp end-location dst)
	(progn
	  (dev:govern eng 0.0)
	  (finish-command agent cmd :end-time end-time))
	(progn
	  (push-action agent (make-action :fn #'(lambda () (exec-move agent cmd eng))
					  :scope :global
					  :description :move))))
    end-time))
