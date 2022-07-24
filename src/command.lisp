(defpackage alaman.command
  (:use #:cl #:alaman.core)
  (:export #:no-op
	   #:move-to
	   #:sleep-until
	   #:explore
	   #:state
	   #:param
	   #:cvar
	   #:donep))
(in-package :alaman.command)

(defun no-op ()
  (make-command :id (new-id)
		:kind :no-op
		:state :new))

(defun move-to (location)
  (make-command :id (new-id)
		:kind :move
		:state :new
		:params (list (cons :location location))))

(defun sleep-until (until)
  (make-command :id (new-id)
		:kind :sleep
		:state :new
		:params (list (cons :until until))))

(defun explore (&optional bounds &key (end-time nil))
  "bounds: bounding rectangle '((x1 y1) (x2 y2))"
  (make-command :id (new-id)
		:kind :explore
		:state :new
		:params (list (cons :bounds bounds) (cons :end-time end-time))))

(defun gather (quantity resource &key (end-time nil))
  (make-command :id (new-id)
		:kind :explore
		:state :new
		:params (list (cons :quantity quantity) (cons :resource resource)
			      (cons :end-time end-time))))

(defun state (cmd)
  (cmd-state cmd))

(defun param (cmd name)
  (cdr (assoc name (cmd-params cmd))))

(defun cvar (cmd name)
  (cdr (assoc name (cmd-vars cmd))))

(defun donep (cmd)
  (equalp :done (state cmd)))
