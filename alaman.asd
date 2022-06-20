(defsystem "alaman"
  :version "0.1.0"
  :author "Alex Steele"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "alaman/tests")))
  :entry-point "alaman:main")

(defsystem "alaman/tests"
  :author ""
  :license ""
  :depends-on ("alaman"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for alaman"
  :perform (test-op (op c) (symbol-call :rove :run c)))
