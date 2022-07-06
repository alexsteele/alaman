(defpackage alaman.map
  (:use #:cl #:alaman.core)
  (:export #:generate-map
	   #:uniform-map
	   #:fill-tiles
	   #:tile-kinds))
(in-package alaman.map)

(defvar *all-tile-kinds* '(water grass wheat rock))

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

(defun fill-tiles (m kind)
  (fill-map m #'(lambda () (make-tile :kind kind))))

(defun uniform-map (dimensions &optional kind)
  (fill-tiles (make-array dimensions) (or kind :grass)))

(defun map-array (m fn)
  (let ((result (make-array (array-dimensions m))))
    (dotimes (i (array-total-size m))
      (setf (row-major-aref result i)
	    (funcall fn (row-major-aref m i))))
    result))

(defun tile-kinds (m)
  (map-array m #'tile-kind))
