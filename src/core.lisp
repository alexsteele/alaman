(defpackage alaman.core
  (:use #:cl))
(in-package :alaman.core)

(defstruct spec
  "Simulation specification."
  (name nil)
  (folder nil)
  (rng nil)
  (universe nil)
  (admin nil)
  (agents nil)
  (nameserver nil)
  (clock nil))

(defstruct universe
  "A map of the universe."
  (dims '(1000 1000))
  (tiles nil)
  (entities nil))

(defstruct tile
  "A tile in the universe."
  (kind nil)
  (climate nil)
  (location nil)
  (entities nil))

(defun tile-at (uni row col)
  (aref (universe-tiles uni) row col))

(defun tile-at-loc (uni location)
  "location: '(row col)"
  (destructuring-bind (row col) location
    (tile-at uni row col)))

(defstruct object
  "An object in the universe."
  (id "")
  (name "")
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
  (last-seen nil))

(defstruct device-info
  "Information about a device."
  (id "")
  (name "")
  (kind nil)
  (agent-id nil)
  (state nil)
  (tags nil)
  (settings nil)
  (vars nil))

(defstruct command
  "A command to be performed by an agent."
  (id "")
  (agent-id "")
  (kind nil)
  (state nil)
  (params nil)
  (run-after nil)
  (start-time nil)
  (end-time nil)
  (result nil))

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
       #'(lambda (x) (rand-select *charset*))
       (make-string length)))

(defun rand-select (seq)
  (elt seq (random (length seq))))

(defun export-all (pkg)
  (let ((pack (find-package pkg)))
    (do-symbols (sym pack)
      (when (eql (symbol-package sym) pack) (export sym)))))
(export-all :alaman.core)
