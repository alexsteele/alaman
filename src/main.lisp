(defpackage alaman
  (:use :cl)
  (:import-from #:alaman.core
		#:alaman.admin
		#:alaman.agent))
(in-package :alaman)

(print "hello world")
(print (admin-init))
(print (alaman.core::make-agent))
(print (alaman.core::make-universe))
