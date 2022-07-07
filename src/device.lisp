(defpackage alaman.device
  (:use #:cl)
  (:import-from :alaman.core)
  (:import-from :alexandria :when-let)
  (:local-nicknames
   (:core :alaman.core)))
;; Note: export-all below
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

(defmethod replenish (battery n)
  (setf (level battery) (min (+ n (level battery))
			     (capacity battery))))

(defmethod consume (battery n)
  (assert (>= (level battery) n))
  (decf (level battery) n))

(defmethod dostep ((b battery) elapsed-sec)
  (consume b (float (* (drain-rate b) elapsed-sec))))

(defclass solar-panel (device)
  ((battery
    :initarg :battery
    :accessor battery)
   (universe
    :initarg :universe
    :reader universe)
   (fill-rate
    :initarg :fill-rate
    :reader fill-rate)))

(defun new-solar-panel (&key location universe (fill-rate 1.0) (battery nil))
  (assert location)
  (assert universe)
  (make-instance 'solar-panel :location location
			      :universe universe
			      :fill-rate fill-rate
			      :battery battery))

(defmethod attach ((panel solar-panel) battery)
  (setf (battery panel) battery))

(defmethod detach ((panel solar-panel))
  (setf (battery panel) nil))

(defmethod sunnyp ((panel solar-panel))
  (let* ((uni (universe panel))
	 (loc (location panel))
	 (tile (core:tile-at-loc uni loc))
	 (climate (core:tile-climate tile)))
    (equalp :sunny climate)))

(defmethod dostep ((panel solar-panel) elapsed-sec)
  (when (and (battery panel) (sunnyp panel))
    (replenish (battery panel) (float (* (fill-rate panel) elapsed-sec)))))

(core:export-all :alaman.device)
