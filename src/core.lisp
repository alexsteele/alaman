(defpackage alaman.core
  (:use #:cl))
(in-package :alaman.core)

(defstruct world
  "A map of the world."
  (dims '(1000 1000))
  (tiles nil)
  (entities nil))

(defstruct tile
  "A tile in the world."
  (kind nil)
  (weather nil)
  (location nil)
  (entities nil))

(defun tile-at (uni row col)
  (aref (world-tiles uni) row col))

(defun tile-at-loc (uni location)
  "location: '(row col)"
  (destructuring-bind (row col) location
    (tile-at uni row col)))

(defstruct object
  "An object in the world."
  (id nil)
  (name nil)
  (kind nil)
  (tags nil)
  (vars nil)
  (version 0)
  (created-at nil)
  (updated-at nil))

(defstruct agent-info
  "Information about an agent."
  (id "")
  (name "")
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil)
  (location nil)
  (orientation nil)
  (last-seen nil))

(defstruct device-info
  "Information about a device."
  (id nil)
  (name nil)
  (kind nil)
  (agent-id nil)
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil))

(defstruct (command (:conc-name cmd-))
  "A command to be performed by an agent."
  (id nil)
  (agent-id nil)
  (kind nil)
  (state nil)
  (params nil)
  (vars nil)
  (run-after nil)
  (start-time nil)
  (end-time nil)
  (result nil))

(defvar *command-states* '(:new :running :cancelled :done :error))

(defstruct event
  "An event recorded by an agent."
  (id "")
  (kind nil)
  (tags nil)
  (vars nil)
  (timestamp nil))

(defvar *charset* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

;; TODO: Use UUID?
(defun new-id () (rand-string 16))

(defun rand-string (length)
  (map 'string
       #'(lambda (x) (declare (ignore x)) (rand-select *charset*))
       (make-string length)))

(defun rand-select (seq)
  (elt seq (random (length seq))))

(defun rand-range (min max)
  (assert (< min max))
  (+ min (random (- max min))))

(defun rand-range-incl (min max)
  (rand-range min (+ 1 max)))

(defun rand-point (dims)
  (list (random (first dims))
	(random (second dims))))

(defun export-all (pkg)
  (let ((pack (find-package pkg)))
    (do-symbols (sym pack)
      (when (eql (symbol-package sym) pack) (export sym)))))
(export-all :alaman.core)
