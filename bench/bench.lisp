;;;; Some simple benchmarks for dsm
;;;

(org.tfeb.tools.require-module:needs
 (:org.tfeb.dsm :compile t))

(defpackage :org.tfeb.dsm/bench
  (:use :cl :org.tfeb.dsm))

(in-package :org.tfeb.dsm/bench)

(defun make-dsb-bencher (lambda-list body &optional (optimize '() optimizep))
  (let ((<start> (make-symbol "START"))
        (<thing> (make-symbol "THING"))
        (<iterations> (make-symbol "ITERATIONS"))
        (<i> (make-symbol "I")))
    (compile nil
             `(lambda (,<thing> ,<iterations>)
                (declare (type fixnum ,<iterations>))
                ,@(if optimizep
                      `((declare (optimize ,optimize)))
                    ())
                (let ((,<start> (get-internal-run-time)))
                  (dotimes (,<i> ,<iterations> (/ (- (get-internal-run-time)
                                                     ,<start>)
                                                  internal-time-units-per-second))
                    (destructuring-bind ,lambda-list ,<thing>
                      ,@body)))))))

(defun make-dsm-bencher (lambda-list body &optional (optimize '() optimizep))
  (let ((<start> (make-symbol "START"))
        (<thing> (make-symbol "THING"))
        (<iterations> (make-symbol "ITERATIONS"))
        (<i> (make-symbol "I")))
    (compile nil
             `(lambda (,<thing> ,<iterations>)
                (declare (type fixnum ,<iterations>))
                ,@(if optimizep
                      `((declare (optimize ,optimize)))
                    ())
                (let ((,<start> (get-internal-run-time)))
                  (dotimes (,<i> ,<iterations> (/ (- (get-internal-run-time)
                                                     ,<start>)
                                                  internal-time-units-per-second))
                    (destructuring-match ,<thing>
                      (,lambda-list ,@body))))))))

(defun bench (lambda-list body thing n)
  (values (funcall (make-dsb-bencher lambda-list body '(speed (debug 0))) thing n)
          (funcall (make-dsm-bencher lambda-list body '(speed (debug 0))) thing n)))

(defvar *descriptions*
  '(((a b c d) (1 2 3 4) (+ a b c d))
    ((&rest l) (1 2 3 4) l)
    ((&key a b c d) (:a 1 :b 2 :c 3 :d 4) (+ a b c d))
    ((&key a b c d) (:d 1 :c 2 :b 3 :a 4) (+ a b c d))
    ((&key a &allow-other-keys) (:a 1 :b 2 :c 3 :d 4) a)
    ((&key a) (:a 1 :b 2 :c 3 :d 4 :allow-other-keys t) a)
    ((a (b &optional (c 0)) &body forms) (f (v 1) 1 2 3)
     (declare (ignore a b forms)) c)))

(defun bench-many (n &optional (descriptions *descriptions*))
  (mapcar (lambda (description)
            (destructuring-match description
              ((ll thing &body body)
               (multiple-value-bind (dsbt dsmt)
                   (bench ll body thing n)
                 (list ll body thing dsbt dsmt)))
              (otherwise
               (error "bad description"))))
          descriptions))

(defun report-many (n &optional (descriptions *descriptions*))
  (format t "~& ratio~11Tdsbt~20Tdsmt lambda list & arguments (~:D iterations)~%"
          n)
  (dolist (description descriptions (values))
    (destructuring-match description
      ((ll thing &body body)
       (multiple-value-bind (dsbt dsmt)
           (bench ll body thing n)
         (if (or (zerop dsbt) (zerop dsmt))
             (format t "~&  -~7T~8,3F ~8,3F ~S ~S~%"
                     (float dsbt) (float dsmt) ll thing)
           (format t "~&~6,2F ~8,3F ~8,3F ~S ~S~%"
                   (float (/ dsbt dsmt))
                   (float dsbt) (float dsmt)
                   ll thing)))))))

(report-many 100000000)
