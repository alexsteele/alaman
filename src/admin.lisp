(defpackage alaman.admin
  (:use #:cl)
  (:import-from #:alaman.core))

(in-package :alaman.admin)

(defstruct admin
  (root "/tmp/alaman")
  (universe nil)
  (agents (make-hash-table :test #'equal))
  (devices (make-hash-table :test #'equal))
  (command-queue nil)
  (events nil))

(defvar *admin* nil)

(defun admin-init (&key (universe nil) (root nil))
  nil)

(defun admin-start ()
  nil)

(defun admin-stop ()
  nil)

(defun register-agent (info)
  nil)

(defun describe-agent (name)
  nil)

(defun list-agents ()
  nil)

(defun send-command (command)
  nil)

(defun list-commands ()
  nil)

(defun cancel-command (id)
  nil)

(defun agent-check-in (agent-id)
  nil)

(defun agent-log-events (agent-id events)
  nil)
