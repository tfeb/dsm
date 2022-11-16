;;;; DSM test package
;;;

(in-package :cl-user)

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.dsm :compile t)
 ("parachute"))

(defpackage :org.tfeb.dsm/test
  (:use :cl)
  (:use :org.tfeb.dsm/impl
   :org.tfeb.dsm
   :org.shirakumo.parachute))
