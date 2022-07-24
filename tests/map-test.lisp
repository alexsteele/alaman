(defpackage alaman/tests/map
  (:use :cl :fiveam :alaman.map :alaman.core)
  (:import-from :serapeum :dict :href))
(in-package :alaman/tests/map)

(def-suite* map-tests
  :description "map tests")

(in-suite map-tests)

(test map-objects
  (is (equalp (wheat 3) (wheat 3)))
  (is (not (equalp (grass 1) (grass 3))))
  (is (equalp 3 (href (water 3) :quantity))))

(test map-basics
  (let ((m (uniform-map '(3 3) :kind :wheat)))
    (is (equalp '(3 3) (array-dimensions m)))
    (is (equalp (make-tile :kind :wheat :weather :sunny) (aref m 0 0)))

    (fill-tiles m :kind :grass :weather :cloudy)

    (is (equalp (make-tile :kind :grass :weather :cloudy) (aref m 0 0)))
    (is (equalp (make-array '(3 3) :initial-element :grass) (tile-kinds m)))))

(test init-from-tile-kinds
  (let* ((kinds #2A('(grass grass grass)
                    '(wheat grass wheat)
		    '(water wheat rock)))
	(m (init-from-tile-kinds kinds)))
    (is (equalp (tile-kinds m) kinds))))

(run! 'map-tests)
