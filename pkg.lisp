;;;; Package for DSM
;;;
;;; (very preliminary)

(in-package :cl-user)

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.simple-loops
   :org.tfeb.hax.utilities
   :org.tfeb.hax.spam)
  :compile t))

(defpackage :org.tfeb.dsm/impl
  (:use :cl)
  (:use
   :org.tfeb.tools.require-module
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.simple-loops
   :org.tfeb.hax.utilities
   :org.tfeb.hax.spam)
  (:export
   #:dsm-error
   #:dsm-error/yours
   #:dsm-error/mine
   #:scold
   #:catastrophe
   #:parse-lambda-list
   #:compile-parsed-lambda-list))

(defpackage :org.tfeb.dsm
  (:use :cl)
  (:use
   :org.tfeb.tools.require-module
   :org.tfeb.hax.collecting
   :org.tfeb.hax.utilities
   :org.tfeb.hax.simple-loops
   :org.tfeb.hax.spam
   :org.tfeb.dsm/impl)
  (:export
   #:dsm-error
   #:dsm-error/yours
   #:dsm-error/mine
   #:destructuring-match))

