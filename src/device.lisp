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

(defmethod dostep (device elapsed-sec)
  "Advance the device state by elapsed-sec."
  nil)

(defclass thermometer (device) ())

(defmethod read-temp (thermometer) nil)

(defclass battery (device)
  ((capacity
    :initarg :capacity
    :accessor capacity)
   (level
    :initarg :level
    :accessor level)
   (drain-rate
    :initarg :drain-rate ; per second
    :reader drain-rate)))

(defun new-battery (&key (drain-rate 0))
  (make-instance 'battery :capacity 100 :level 100 :drain-rate drain-rate))

(defmethod consume (battery n)
  (assert (>= (level battery) n))
  (decf (level battery) n))

(defmethod dostep ((b battery) elapsed-sec)
  (consume b (float (* (drain-rate b) elapsed-sec))))

(defclass solar-panel (device) ())
