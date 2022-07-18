(defpackage alaman/tests/device
  (:use :cl :alaman :fiveam :alaman.core)
  (:import-from :alaman.device)
  (:import-from :alaman.map)
  (:local-nicknames
   (:dev :alaman.device)
   (:am :alaman.map)))
(in-package :alaman/tests/device)

(def-suite* device-tests
  :description "device tests")

(in-suite device-tests)

(test battery-test
  (let ((bat (dev:new-battery)))
    (is (= 100 (dev:capacity bat)))
    (is (= 100 (dev:level bat)))
    (dev:consume bat 60)
    (is (= 40 (dev:level bat)))
    (dev:replenish bat 100) ; fills to capacity=100
    (is (= 100 (dev:level bat)))))

(test solar-panel-test
  (let* ((tiles (am:uniform-map '(1 1) :weather :sunny))
	 (U (make-universe :tiles tiles))
	 (panel (dev:new-solar-panel :location '(0 0)
				     :universe U
				     :fill-rate 1.0))
	 (bat (dev:new-battery)))

    (is (null (dev:battery panel)))
    (dev:detach panel) ; no-op

    (dev:run-step panel 10)

    (dev:attach panel bat)
    (dev:consume bat 50)

    ; sunny? replenish battery
    (dev:run-step panel 10)
    (is (equalp 60.0 (dev:level bat)))

    ; cloudy? no change
    (am:fill-tiles tiles :weather :cloudy)
    (dev:run-step panel 10)
    (is (equalp 60.0 (dev:level bat)))))

(test engine-test
  (let* ((bat (dev:new-battery))
	 (eng (dev:new-engine :battery bat)))

    (is (= (dev:output eng) 0.0))
    (is (= (dev:thrust eng) 0.0))

    ;; No output
    (dev:govern eng 0.0)
    (dev:run-step eng 1.0)
    (is (= (dev:thrust eng) 0.0))
    (is (= (dev:level bat) 100.0))

    ;; Max output
    (dev:govern eng 1.0)
    (is (= (dev:thrust eng) 100.0))
    (dev:run-step eng 1.0)
    (is (= (dev:level bat) 99.0))
    (dev:run-step eng 9.0)
    (is (= (dev:level bat) 90.0))

    ;; Consume entire battery. Output should be adjusted.
    (dev:run-step eng 100.0)
    (is (= (dev:output eng) 0.9)) ; elapsed-sec / battery level
    (is (= (dev:thrust eng) 90.0)) ; .9 * max-thrust

    ;; One more step.
    (dev:run-step eng 1.0)
    (is (= (dev:output eng) 0.0))
    (is (= (dev:thrust eng) 0.0))))

(run! 'device-tests)
