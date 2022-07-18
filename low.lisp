;;;; DSM low-level
;;;

(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.dsm/impl)

;;; Conditions
;;;

(define-condition dsm-error (error)
  ())

(define-condition dsm-error/yours (dsm-error)
  ;; Errors which I think are not my fault
  ())

(define-condition dsm-error/mine (dsm-error)
  ;; Errors which are my fault
  ())

(define-condition catastrophe (dsm-error/mine simple-error)
  ())

(defun catastrophe (control &rest arguments)
  ;; Something bad which is my fault
  (error 'catastrophe
         :format-control control
         :format-arguments arguments))

(define-condition scold (dsm-error/yours simple-error)
  ()) 

(defun scold (control &rest arguments)
  ;; Something bad which is not really my fault (at least: it is nt
  (error 'scold
         :format-control control
         :format-arguments arguments))
