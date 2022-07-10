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

(defmethod dostep ((dev device) elapsed-sec)
  "Advance the device state by elapsed-sec."
  dev)

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

(defun new-battery (&key (drain-rate 0.0))
  (make-instance 'battery :capacity 100 :level 100 :drain-rate drain-rate))

(defmethod replenish (battery n)
  (setf (level battery) (min (+ n (level battery))
			     (capacity battery))))

(defmethod consume (battery n)
  (assert (>= (level battery) n))
  (decf (level battery) n))

(defmethod dostep ((b battery) elapsed-sec)
  (consume b (float (* (drain-rate b) elapsed-sec)))
  b)

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
    (replenish (battery panel) (float (* (fill-rate panel) elapsed-sec))))
  panel)

;; A battery powered engine. An engine consumes energy from a battery
;; and outputs thrust (or force) to perform work. Parameters:
;;
;;  battery: battery that the engine draws from
;;  output: current output level between [0, 1]
;;  thrust: thrust at current output between [0, max-thrust]
;;  max-drain-rate: battery consumption rate at output 1.0
;;  max-thrust: thrust at output 1.0
(defclass engine (device)
  ((battery
    :initarg :battery
    :accessor battery)
   (output
    :initarg :output
    :accessor output)
   (max-drain-rate
    :initarg :max-drain-rate
    :reader max-drain-rate)
   (max-thrust
    :initarg :max-thrust
    :reader max-thrust)))

(defun new-engine (&key battery (max-drain-rate 1.0) (max-thrust 100.0))
  (make-instance 'engine :battery battery
			 :output 0.0
			 :max-drain-rate max-drain-rate
			 :max-thrust max-thrust))

(defmethod dostep ((eng engine) elapsed-sec)
  (let* ((B (battery eng))
	 (R (* (max-drain-rate eng) (output eng)))
	 (N (* R elapsed-sec))
	 (L (level B)))
    (if (<= N L)
	;; Enough charge. Maintain output.
	(consume B N)
	;; Not enough charge.
	;; Adjust output to drain the battery over `elapsed-sec`.
	;;  Let R = battery drain rate = max-drain-rate * output
	;;  Target is R = L / elapsed-sec
	;;  implies R = L / elapsed-sec = max-drain-rate * output
	;;  implies output = L / (elapsed-sec * max-drain-rate)
	(progn
	  (setf (output eng) (/ L (* elapsed-sec (max-drain-rate eng))))
	  (consume B L))))
  eng)

(defmethod govern ((eng engine) level)
  "Set the output level of the engine between [0.0, 100.0]"
  (assert (and (<= 0.0 level) (<= level 1.0)))
  (setf (output eng) level))

(defmethod thrust ((eng engine))
  "Returns the force output by the engine."
  (* (output eng) (max-thrust eng)))

;; TODO: implement
(defclass thermometer (device) ())

(defmethod read-temp (thermometer) nil)

(core:export-all :alaman.device)
