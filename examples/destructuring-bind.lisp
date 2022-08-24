;;;; An implementation of DESTRUCTURING-BIND
;;;
;;; It's perfectly easy to implement DESTRUCTURING-BIND in terms of
;;; DESTRUCTURING-MATCH: this is an implementation directly in terms
;;; of the *implementation* of DESTRUCTURING-MATCH.
;;;

(in-package :cl-user)

(needs ((:org.tfeb.dsm
         :org.tfeb.conduit-packages)
        :compile t))

(in-package :org.tfeb.clc-user)

(defpackage :org.tfeb.play.cl/dsb
  ;; This is CL but with this implementation of DESTRUCTURING-BIND
  (:use :org.tfeb.dsm/impl)
  (:extends/excluding :cl #:destructuring-bind)
  (:export #:destructuring-bind))

(in-package :org.tfeb.play.cl/dsb)

(define-condition dsb-error (program-error simple-error)
  ())

(defmacro destructuring-bind (lambda-list form &body decls/forms)
  `(funcall
    ,(compile-lambda-list lambda-list decls/forms)
    ,form
    (lambda (message arguments)
      (error 'dsb-error
             :format-control message
             :format-arguments arguments))))
