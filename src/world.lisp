(defpackage alaman.world
  (:use #:cl #:alaman.core)
  (:import-from :spinneret)
  (:import-from :serapeum :dict :href)
  (:local-nicknames
   (:core :alaman.core)
   (:sp :spinneret))
  (:export #:water
	   #:wheat
	   #:grass
	   #:rock
	   #:oil
	   #:generate-map
	   #:uniform-map
	   #:fill-tiles
	   #:init-from-tile-kinds
	   #:tile-kinds
	   #:render-html))
(in-package alaman.world)

(defvar *all-tile-kinds* '(water grass wheat rock))
(defvar *all-weathers* '(:sunny :cloudy :rainy))

;;; Objects

(defun water (&optional (quantity 1))
  (dict :kind :water :quantity quantity))

(defun wheat (&optional (quantity 1))
  (dict :kind :wheat :quantity quantity))

(defun grass (&optional (quantity 1))
  (dict :kind :grass :quantity quantity))

(defun rock (&optional (quantity 1))
  (dict :kind :rock :quantity quantity))

(defun oil (&optional (quantity 1))
  (dict :kind :oil :quantity quantity))

(defun wood (&optional (quantity 1))
  (dict :kind :wood :quantity 1))

(defun iron (&optional (quantity 1))
  (dict :kind :iron :quantity 1))

;;; Tiles

(defun water-tile ()
  (make-tile :kind :water
	     :entities (list (water))))

(defun wheat-tile ()
  (make-tile :kind :wheat
	     :entities (list (wheat))))

(defun grass-tile ()
  (make-tile :kind :grass
	     :entities (list (grass))))

(defun rock-tile ()
  (make-tile :kind :rock
	     :entities (list (rock))))

(defun make-tile-kind (kind)
  (case kind
    (:water (water-tile))
    (:grass (grass-tile))
    (:wheat (wheat-tile))
    (:rock (rock-tile))
    (t (error "unrecognized tile kind"))))

(defun rainyp (tile)
  (equalp (tile-weather tile) :rainy))

(defun sunnyp (tile)
  (equalp (tile-weather tile) :sunny))

(defun cloudyp (tile)
  (equalp (tile-weather tile) :cloudy))

;; Map

(defun fill-map (m fn)
  (dotimes (i (array-total-size m))
    (setf (row-major-aref m i) (funcall fn)))
  m)

(defun fill-tiles (m &key (kind :grass) (weather :sunny))
  (fill-map m #'(lambda () (make-tile :kind kind
				      :weather weather))))

(defun uniform-map (dimensions &key (kind :grass) (weather :sunny))
  (fill-tiles (make-array dimensions) :kind kind
				      :weather weather))

(defun map-array (m fn)
  (let ((result (make-array (array-dimensions m))))
    (dotimes (i (array-total-size m))
      (setf (row-major-aref result i)
	    (funcall fn (row-major-aref m i))))
    result))

(defun init-from-tile-kinds (kinds)
  (map-array kinds #'(lambda (k) (make-tile :kind k))))

(defun tile-kinds (m)
  (map-array m #'tile-kind))

(defun render-html (tiles)
  ;; TODO: Consider adding an on-hover so you can see the contents of a tile.
  (let* ((dims (array-dimensions tiles))
	 (rows (first dims))
	 (cols (second dims)))
  (sp:with-html
    (:table
     (dotimes (row rows)
       (:tr (dotimes (col cols)
	      (let ((tile (aref tiles row col)))
		(:td :class (tile-html-class tile))))))))))

(defun tile-html-class (tile)
  (case (core:tile-kind tile)
    (:wheat "wheat tile")
    (:grass "grass tile")
    (:water "water tile")
    (:rock "rock tile")
    (t (error "unrecognized tile kind"))))
