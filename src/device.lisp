(defpackage alaman.device
  (:use #:cl))
(in-package :alaman.device)

(defclass device ()
  ((info
    :initarg :info
    :accessor info)
   (location
    :initarg :location
    :accessor location)))

(defclass thermometer (device) ())

(defmethod read-temp (thermometer) nil)

(defclass battery (device) ())

(defclass solar-panel (device) ())
