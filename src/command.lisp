(defpackage alaman.command
  (:use #:cl #:alaman.core)
  (:export #:move-to
	   #:sleep-until))
(in-package :alaman.command)

(defun move-to (location)
  (make-command :id (new-id)
		:kind :move
		:state :start
		:params (list :location location)))

(defun sleep-until (until)
  (make-command :id (new-id)
		:kind :sleep
		:state :start
		:params (list :until until)))
