(defpackage alaman.map
  (:use #:cl #:alaman.core)
  (:import-from :spinneret)
  (:local-nicknames
   (:core :alaman.core)
   (:sp :spinneret))
  (:export #:generate-map
	   #:uniform-map
	   #:fill-tiles
	   #:init-from-tile-kinds
	   #:tile-kinds
	   #:render-html))
(in-package alaman.map)

(defvar *all-tile-kinds* '(water grass wheat rock))
(defvar *all-weathers* '(:sunny :cloudy :rainy))

(defun water ()
  (make-tile :kind :water
	     :entities '((water quantity 10))))

(defun grass ()
  (make-tile :kind :grass))

(defun wheat ()
  (make-tile :kind :wheat
	     :entities '((wheat quantity 10))))

(defun rock ()
  (make-tile :kind :rock
	     :entities '((rock quantity 10))))

(defun make-tile-kind (kind)
  (case kind
    (:water (water))
    (:grass (grass))
    (:wheat (wheat))
    (:rock (rock))
    (t (error "unrecognized tile kind"))))

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
