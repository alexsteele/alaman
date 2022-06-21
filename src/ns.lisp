;; Name service
(defpackage alaman.ns
  (:use #:cl)
  (:nicknames "ns")
  (:export #:init
	   #:register
	   #:lookup
	   #:connect
	   #:children))

(in-package :alaman.ns)

(defun init () (make-hash-table :test #'equal))

(defun register (ns name obj &optional data)
  (setf (gethash name ns) (list obj data)))

(defun connect (ns name)
  (first (gethash name ns '(nil nil))))

(defun lookup (ns name)
  (second (gethash name ns '(nil nil))))

;;; TODO: Add prefix filter
(defun children (ns name)
  "Returns a list of '(<name> <obj> <data>)"
  (let* ((entries nil))
    (maphash #'(lambda (name entry) (push (append (list name) entry) entries))
	     ns)
    entries))

