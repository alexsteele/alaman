(defpackage alaman/tests/map
  (:use :cl :alaman :fiveam :alaman.map :alaman.core))
(in-package :alaman/tests/map)

(def-suite* map-tests
  :description "map tests")

(in-suite map-tests)

(test map-basics
  (let ((m (uniform-map '(3 3) :kind :wheat)))
    (is (equalp '(3 3) (array-dimensions m)))
    (is (equalp (make-tile :kind :wheat :climate :sunny) (aref m 0 0)))

    (fill-tiles m :kind :grass :climate :cloudy)

    (is (equalp (make-tile :kind :grass :climate :cloudy) (aref m 0 0)))
    (is (equalp (make-array '(3 3) :initial-element :grass) (tile-kinds m)))))

(run! 'map-tests)
