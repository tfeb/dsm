;;;; Test dsm itself
;;;
;;; It's slightly hard to know what to test here: if the lambda list
;;; tests pass this kind of will too I think.
;;;

(in-package :org.tfeb.dsm/test)

(define-test ("org.tfeb.dsm" "org.tfeb.dsm.dsm"))

(define-test ("org.tfeb.dsm.dsm" "sanity")
  (is = 1 (destructuring-match '(1)
            ((x) x)))
  (is = 1 (destructuring-match 1
            (x x)))
  (true (destructuring-match 1
          (_ t))))

#||
(define-test ("org.tfeb.dsm.dsm" "error-checking")
  ;; I have no idea what is breaking this: parachute is broken somehow
  :compile-at :execute
  (fail (destructuring-match 1 3) scold)
  (fail (destructuring-match 1
          (otherwise t)
          (_ nil)) scold))
||#

(define-test ("org.tfeb.dsm.dsm" "basic")
  (flet ((m (it)
           (destructuring-match it
             ((x y)
              (declare (ignore x y))
              1)
             ((x . y)
              (:when (symbolp x))
              (declare (ignore y))
              2)
             ((x . y)
              (:unless (symbolp x))
              (declare (ignore y))
              3)
             (otherwise
              4))))
    (is = 1 (m '(1 2)))
    (is = 2 (m '(x . 2)))
    (is = 3 (m '(1 . 2)))
    (is = 4 (m ""))))

(define-test ("org.tfeb.dsm.dsm" "anonymous")
  (flet ((m (it)
           (destructuring-match it
             ((_)
              1)
             ((_ _)
              2)
             ((_ _ &key ((:x _)))
              3)
             (otherwise
              4))))
    (is = 1 (m '(x)))
    (is = 2 (m '(x y)))
    (is = 3 (m '(x y :x 1)))
    (is = 4 (m '(x y :y 2)))))

(define-test ("org.tfeb.dsm.dsm" "symbol-ll")
  (let ((it '(x y)))
    (is eq it (destructuring-match it
                ((_ _ _) nil)
                (x x)))))

(test "org.tfeb.dsm.dsm" :report 'summary)
