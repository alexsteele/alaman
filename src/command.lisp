(defpackage alaman.command
  (:use #:cl #:alaman.core)
  (:export #:no-op
	   #:move-to
	   #:sleep-until
	   #:state
	   #:param
	   #:cvar))
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

(defun state (cmd)
  (command-state cmd))

(defun param (cmd name)
  (cdr (assoc name (command-params cmd))))

(defun cvar (cmd name)
  (cdr (assoc name (command-vars cmd))))
