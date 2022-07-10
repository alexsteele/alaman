(defpackage alaman.command
  (:use #:cl #:alaman.core)
  (:export #:no-op
	   #:move-to
	   #:sleep-until
	   #:param))
(in-package :alaman.command)

(defun no-op ()
  (make-command :id (new-id)
		:kind :no-op
		:state :start))

(defun move-to (location)
  (make-command :id (new-id)
		:kind :move
		:state :start
		:params (list (cons :location location))))

(defun sleep-until (until)
  (make-command :id (new-id)
		:kind :sleep
		:state :start
		:params (list (cons :until until))))

(defun param (cmd name)
  (cdr (assoc name (command-params cmd))))
