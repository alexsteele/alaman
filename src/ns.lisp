;; The name service allows agents to register their info for the admin
(defpackage alaman.ns
  (:use #:cl)
  (:import-from :alexandria :hash-table-values)
  (:import-from :uiop :string-prefix-p)
  (:nicknames "ns")
  (:export #:init
	   #:register
	   #:existsp
	   #:lookup
	   #:connect
	   #:children
	   #:entry
	   #:make-entry
	   #:entry-name
	   #:entry-data))

(in-package :alaman.ns)

(defstruct entry
  (name nil)
  (obj nil)
  (data nil))

(defun init () (make-hash-table :test #'equal))

(defun register (ns name obj &optional data)
  (setf (gethash name ns) (make-entry :name name :obj obj :data data)))

(defun connect (ns name)
  (entry-obj (gethash name ns (make-entry))))

(defun existsp (ns name)
  (if (gethash name ns) t nil))

(defun lookup (ns name)
  (entry-data (gethash name ns (make-entry))))

(defun entries (ns)
  (hash-table-values ns))

(defun entry-is-child (name)
  (lambda (entry)
    (string-prefix-p name (entry-name entry))))

(defun children (ns name)
  "Returns a list of ns:entry"
  (remove-if-not (entry-is-child name) (entries ns)))
