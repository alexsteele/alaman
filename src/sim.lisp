(defpackage alaman.sim
  (:use #:cl)
  (:export #:init
	   #:start
	   #:run
	   #:dostep
	   #:stop
	   #:save
	   #:load-from))
(in-package :alaman.sim)

(defstruct sim
  "Simulation state."
  (id "")
  (spec nil)
  (universe nil)
  (admin nil)
  (agents nil))

(defun init (spec))
(defun start (sim) nil)
(defun run (sim) nil)
(defun dostep (sim) nil)
(defun stop (sim) nil)
(defun save (sim &optional path) nil)
(defun load-from (path) nil)
