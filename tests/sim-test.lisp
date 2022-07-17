(defpackage alaman/tests/admin
  (:use #:alaman.time)
  (:import-from :alaman.sim)
  (:import-from :alaman.core)
  (:import-from :alaman.ns)
  (:import-from :alaman.map)
  (:local-nicknames
   (:admin :alaman.admin)
   (:core :alaman.core)
   (:ns :alaman.ns)
   (:sim :alaman.sim)
   (:am :alaman.map))
  (:use :cl :fiveam :alaman.core))
(in-package :alaman/tests/admin)

(def-suite* sim-tests
  :description "sim tests")

(in-suite sim-tests)

(defvar *spec* (core:make-spec))
(defvar *sim* (sim:init *spec*))

(test sim-lifecycle
  (sim:start *sim*)
  (sim:run-step *sim*)
  (sim:run-step *sim*)
  (sim:stop *sim*))

(run! 'sim-tests)
