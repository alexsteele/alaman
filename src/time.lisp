(defpackage alaman.time
  (:use #:cl)
  (:nicknames "time")
  (:export #:new-system-clock
	   #:new-fixed-clock
	   #:new-rate-clock
	   #:clock-time
	   #:clock-tick
	   #:clock-set
	   #:clock-pin
	   #:clock-pinp
	   #:clock-unpin))
(in-package :alaman.time)

(defclass clock ()
  ((pin	;; pinned time or nil
    :initform nil
    :accessor pin)))

(defun new-system-clock ()
  "A default clock that returns the system time. Cannot be set."
  (make-instance 'clock))
(defmethod clock-time (clock) (get-universal-time))
(defmethod clock-tick (clock) (clock-time clock))
(defmethod clock-set (clock time) (clock-time clock))
(defmethod clock-pin (clock)
  "Pin the clock at the current time.
While pinned, clock-time will return the pinned time, but clock-tick
may still advance the clock's internal state."
  (setf (pin clock) (clock-time clock)))
(defmethod clock-pinp (clock) (not (null (pin clock))))
(defmethod clock-unpin (clock)
  (setf (pin clock) nil))

(defclass fixed-clock (clock)
  ((val
    :initarg :val
    :accessor val)
   (tick-amount
    :initarg :tick-amount
    :accessor tick-amount)))

(defun new-fixed-clock (init-val tick-amount)
  (make-instance 'fixed-clock :val init-val
			      :tick-amount tick-amount))

(defmethod clock-tick ((clock fixed-clock))
  (setf (val clock) (+ (val clock) (tick-amount clock)))
  (val clock))

(defmethod clock-time ((clock fixed-clock))
  (val clock))

(defclass rate-clock (clock)
  ((val
    :initarg :val
    :accessor val)
   (tick-amount
    :initarg :tick-amount
    :accessor tick-amount)
   (tick-freq
    :initarg :tick-freq
    :accessor tick-freq)
   (last-tick
    :initarg :last-tick
    :accessor last-tick)))

(defun new-rate-clock (&key (init-value nil) (tick-amount 1) (tick-freq 1))
  "A fixed rate clock that advances by tick-amount every tick-freq seconds."
  (let ((now (get-universal-time)))
   (make-instance 'rate-clock :val (or init-value now)
			       :tick-amount tick-amount
			       :tick-freq tick-freq
			       :last-tick now)))

(defmethod clock-time ((clock rate-clock))
  (clock-tick clock))

(defmethod clock-tick ((clock rate-clock))
  (let* ((now (get-universal-time))
	 (elapsed-sec (- now (last-tick clock)))
	 (ticks-per-sec (/ (tick-amount clock) (tick-freq clock)))
	 (advance-by (* elapsed-sec ticks-per-sec))
	 (new-val (+ (val clock) advance-by)))
    (setf (val clock) new-val)
    (setf (last-tick clock) now))
  (val clock))

(defmethod clock-set ((clock rate-clock) time)
  (setf (last-tick clock) (get-universal-time))
  (setf (val clock) time))
