;; The name service allows agents to register their info for the admin
(defpackage alaman.ns
  (:use #:cl)
  (:import-from :alexandria :starts-with-subseq)
  (:nicknames "ns")
  (:export #:init
	   #:register
	   #:lookup
	   #:connect
	   #:children
	   #:entry
	   #:make-entry
	   #:entry-name
	   #:entry-data))

(in-package :alaman.ns)

(defstruct entry
  (name "")
  (obj nil)
  (data nil))

(defun init () (make-hash-table :test #'equal))

(defun register (ns name obj &optional data)
  (setf (gethash name ns) (list obj data)))

(defun connect (ns name)
  (first (gethash name ns '(nil nil))))

(defun lookup (ns name)
  (second (gethash name ns '(nil nil))))

;;; TODO: There must be a better way to do this...
(defun entries (ns)
  (let ((result nil))
    (maphash #'(lambda (name obj-data)
		 (push (make-entry :name name
				   :obj (first obj-data)
				   :data (second obj-data))
		       result))
	     ns)
    result))

(defun children (ns name)
  "Returns a list of ns:entry"
  (remove-if #'(lambda (entry)
		 (print (starts-with-subseq name (entry-name entry)))
		 (starts-with-subseq name (entry-name entry)))
	     (entries ns)))
