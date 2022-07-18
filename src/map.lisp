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
	   #:tile-kinds))
(in-package alaman.map)

(defvar *all-tile-kinds* '(water grass wheat rock))
(defvar *all-climates* '(:sunny :cloudy :rainy))

(defun water ()
  (make-tile :kind :water))

(defun grass ()
  (make-tile :kind :grass))

(defun wheat ()
  (make-tile :kind :wheat))

(defun rock ()
  (make-tile :kind :rock))

(defun fill-map (m fn)
  (dotimes (i (array-total-size m))
    (setf (row-major-aref m i) (funcall fn)))
  m)

(defun fill-tiles (m &key (kind :grass) (climate :sunny))
  (fill-map m #'(lambda () (make-tile :kind kind
				      :climate climate))))

(defun uniform-map (dimensions &key (kind :grass) (climate :sunny))
  (fill-tiles (make-array dimensions) :kind kind
				      :climate climate))

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
		(:td :class (tile-class tile))))))))))

(defun tile-class (tile)
  (case (core:tile-kind tile)
    (:wheat "wheat")
    (:grass "grass")
    (:water "water")
    (:rock "rock")
    (t (error "unrecognized tile kind"))))
