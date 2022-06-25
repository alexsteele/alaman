(require :asdf)

;; TODO: Get defsystem working on mac.
(asdf:defsystem "alaman"
  :version "0.1.0"
  :author "Alex Steele"
  :license ""
  :depends-on (:alexandria)
  :components ((:module "src"
                :components
                ((:file "main")
		 (:file "core")
		 (:file "ns")
		 (:file "agent")
		 (:file "admin"))))
  :description ""
  :in-order-to ((test-op (test-op "alaman/tests")))
  :entry-point "alaman:main")

(asdf:defsystem "alaman/tests"
  :author ""
  :license ""
  :depends-on ("alaman"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "ns"))))
  :description "Test system for alaman"
  :perform (test-op (op c) (symbol-call :rove :run c)))
