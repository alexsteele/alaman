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

(test battery
  (let ((b (dev:new-battery)))
    (is (= 100 (dev:capacity b)))
    (is (= 100 (dev:level b)))
    (dev:consume b 60)
    (is (= 40 (dev:level b)))
'    (dev:replenish b 100) ; fills to capacity=100
    (is (= 100 (dev:level b)))))

(test solar-panel
  (let* ((tiles (am:uniform-map '(1 1) :climate :sunny))
	 (U (make-universe :tiles tiles))
	 (panel (dev:new-solar-panel :location '(0 0)
				     :universe U
				     :fill-rate 1.0))
	 (bat (new-battery)))

    (is (null (dev:battery panel)))
    (dev:detach panel) ; no-op

    (dev:dostep panel 10)

    (dev:attach panel bat)
    (dev:consume bat 50)

    ; sunny? replenish battery
    (dev:dostep panel 10)
    (is (equalp 60.0 (dev:level bat)))

    ; cloudy? no change
    (fill-tiles tiles :climate :cloudy)
    (dev:dostep panel 10)
    (is (equalp 60.0 (dev:level bat)))))

(run! 'device-tests)
