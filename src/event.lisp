(defpackage alaman.event
  (:use #:cl #:alaman.core)
  (:export #:start-event
	   #:done-event))
(in-package :alaman.event)

(defun start-event (cmd)
  (make-event :id (new-id)
	      :kind :start
	      :vars (list :command-id (command-id cmd))
	      :timestamp (command-start-time cmd)))

(defun done-event (cmd)
  (make-event :id (new-id)
	      :kind :done
	      :vars (list :command-id (command-id cmd))
	      :timestamp (command-end-time cmd)))
