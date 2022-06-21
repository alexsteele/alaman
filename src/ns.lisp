;; Name service
(defpackage alaman.ns
  (:use #:cl)
  (:import-from :alexandria :starts-with-subseq)
  (:nicknames "ns")
  (:export #:init
	   #:register
	   #:lookup
	   #:connect
	   #:children))

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

(starts-with-subseq "/dne" "/foo")
(defun test-ns ()
  (let ((serv (init)))
    (register serv "/foo" "a" "b")
    (assert (equal "b" (lookup serv "/foo")))
    (assert (equal  "a" (connect serv "/foo")))
    (assert (equal nil (children serv "/dne")))
    (assert (equal (list (make-entry :name "/foo" :obj "a" :data "b"))
		   (children serv "/")))))
(test-ns)
