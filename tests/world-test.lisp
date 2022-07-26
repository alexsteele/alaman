(defpackage alaman/tests/world
  (:use :cl :fiveam :alaman.world :alaman.core)
  (:import-from :serapeum :dict :href))
(in-package :alaman/tests/world)

(def-suite* world-tests
  :description "world tests")

(in-suite world-tests)

(test world-objects
  (is (equalp (wheat 3) (wheat 3)))
  (is (not (equalp (grass 1) (grass 3))))
  (is (equalp 3 (href (water 3) :quantity))))

(test world-basics
  (let ((m (uniform-map '(3 3) :kind :wheat)))
    (is (equalp '(3 3) (array-dimensions m)))
    (is (equalp (make-tile :kind :wheat :weather :sunny) (aref m 0 0)))

    (fill-tiles m :kind :grass :weather :cloudy)

    (is (equalp (make-tile :kind :grass :weather :cloudy) (aref m 0 0)))
    (is (equalp (make-array '(3 3) :initial-element :grass) (tile-kinds m)))))

(test init-from-tile-kinds
  (let* ((kinds #2A((:grass :grass :grass)
                    (:wheat :grass :wheat)
		    (:water :wheat :rock)))
	(tiles (init-from-tile-kinds kinds)))
    (is (equalp (tile-kinds tiles) kinds))))

(test world-run-step
  (let* ((kinds #2A((:wheat :rock)))
	 (tiles (init-from-tile-kinds kinds))
	 (wheat (first (tile-entities (row-major-aref tiles 0))))
	 (rock (first (tile-entities (row-major-aref tiles 1)))))

    (is (eq 1 (href wheat :quantity)))
    (is (eq 1 (href rock :quantity)))

    (run-step tiles 1)

    (is (eq 2 (href wheat :quantity)))
    (is (eq 1 (href rock :quantity)))))

(debug! 'world-tests)
