(defpackage alaman/tests/admin
  (:import-from :alaman.sim)
  (:import-from :alaman.core)
  (:import-from :alaman.ns)
  (:local-nicknames
   (:admin :alaman.admin)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:sim :alaman.sim))
  (:use :cl :fiveam :alaman.core))
(in-package :alaman/tests/admin)

(def-suite* sim-tests
  :description "sim tests")

(in-suite sim-tests)

(defvar *ns* (ns:init))
(defvar *clock* (new-system-clock))
(defvar *spec* nil)
(defvar *sim* (sim:init *spec*))

(test rand-spec
  (sim:init (sim:rand-spec)))

(test sim-start
  (sim:start *sim*))

;(run! 'sim-tests)
