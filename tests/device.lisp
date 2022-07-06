(defpackage alaman/tests/device
  (:use :cl :alaman :fiveam :alaman.core)
  (:import-from :alaman.device)
  (:local-nicknames
   (:dev :alaman.device)))
(in-package :alaman/tests/device)

(def-suite* device-tests
  :description "device tests")

(in-suite device-tests)

(test battery
  (let ((b (dev:new-battery)))
    (is (= 100 (dev:capacity b)))
    (dev:consume b 50)
    (is (= 50 (dev:capacity b)))))

(test solar-panel
  nil)

(run! 'device-tests)
