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

(define-test ("org.tfeb.dsm.dsm" "error-checking")
  ;; This is a horrible hack, but it's the easiest way
  (fail (eval '(destructuring-match 1 3))
        scold)
  (fail (eval '(destructuring-match 1
                 (otherwise t)
                 (_ nil)))
        scold))

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

(define-test ("org.tfeb.dsm.dsm" "semantics")
  (let ((it '(1 2 3 4)))
    (is eq it (destructuring-match it
                ((&rest r) r)))
    (is eq (rest it) (destructuring-match it
                       ((_ &rest r) r)))
    (is eq (rest it) (destructuring-match it
                       ((_ . r) r))))
  (let ((dit '(1 2 . 3)))
    (is eq dit (destructuring-match dit
                 ((&rest r) r)))
    (is eql 3 (destructuring-match dit
              ((_ _ &rest r) r))))
  (is eql 1 (destructuring-match 1
              ((&rest r) r))))

(when *test-individually*
  (test "org.tfeb.dsm.dsm" :report *test-report-class*))
