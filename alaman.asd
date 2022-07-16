(require :asdf)
(require :fiveam)

(asdf:defsystem "alaman"
  :version "0.1.0"
  :author "Alex Steele"
  :license "MIT"
  :depends-on (:alexandria :uiop :priority-queue)
  :pathname "src/"
  :components
  ((:file "admin" :depends-on ("core" "time" "ns"))
   (:file "agent" :depends-on ("core" "command" "device" "event" "time" "ns"))
   (:file "core")
   (:file "command" :depends-on ("core"))
   (:file "device" :depends-on ("core" "map"))
   (:file "event" :depends-on ("core"))
   (:file "ns")
   (:file "time")
   (:file "map" :depends-on ("core"))
   (:file "sim" :depends-on ("admin" "agent" "core" "ns" "time"))
   (:file "main" :depends-on ("sim")))
  :description "agent simulation"
  :in-order-to ((test-op (test-op "alaman/tests")))
  :entry-point "alaman:main")

(asdf:defsystem "alaman/tests"
  :author ""
  :license ""
  :depends-on ("alaman"
	       "fiveam")
  :pathname "tests/"
  :components ((:file "admin-test")
	       (:file "agent-test")
	       (:file "device-test")
	       (:file "map-test")
	       (:file "ns-test")
	       (:file "sim-test"))
  :description "Test system for alaman"
  :perform (test-op (op c) (symbol-call :fiveam :run! c)))

(defsystem "alaman/executable"
  :build-operation program-op
  :build-pathname "alaman"
  :entry-point "alaman::start-alaman"
  :depends-on ("alaman")
  :components ((:file "src/main")))
