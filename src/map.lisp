(defpackage alaman.map
  (:use #:cl #:alaman.core)
  (:export #:generate-map))
(in-package alaman.map)

(defvar *tile-kinds* '(water grass wheat rock))

(defun water ()
  (make-tile :kind :water))

(defun grass ()
  (make-tile :kind :grass))

(defun wheat ()
  (make-tile :kind :wheat))

(defun rock ()
  (make-tile :kind :rock))

(defvar *tiles* (make-array '(3 3)))

(defun fill-map (m fn)
  (destructuring-bind (rows cols) (array-dimensions m)
      (loop for row from 0 below rows do
	(loop for col from 0 below cols do
	  (setf (aref m row col) (funcall fn)))))
  m)

(defun fill-tiles (m kind)
  (fill-map m #'(lambda () (make-tile :kind kind))))

(defun uniform-map (dimensions &optional kind)
  (fill-tiles (make-array dimensions) (or kind :wheat)))
