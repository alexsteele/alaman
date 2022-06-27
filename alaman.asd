(require :asdf)

(asdf:defsystem "alaman"
  :version "0.1.0"
  :author "Alex Steele"
  :license "MIT"
  :depends-on (:alexandria :uiop)
  :components ((:module "src"
                :components
                ((:file "admin")
		 (:file "agent")
		 (:file "core")
		 (:file "main")
		 (:file "ns")
		 (:file "sim"))))
  :description "agent simulation"
  :in-order-to ((test-op (test-op "alaman/tests")))
  :entry-point "alaman:main")

(asdf:defsystem "alaman/tests"
  :author ""
  :license ""
  :depends-on ("alaman"
	       "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main")
		 (:file "ns"))))
  :description "Test system for alaman"
  :perform (test-op (op c) (symbol-call :fiveam :run! c)))
