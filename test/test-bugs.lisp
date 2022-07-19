;;;; Tests that were for bugs
;;;

(in-package :org.tfeb.dsm/test)

(define-test ("org.tfeb.dsm" "org.tfeb.dsm.bugs"))

(define-test ("org.tfeb.dsm.bugs" "null")
  (false (match-one '(n () . more) '(x (y) 1 2)))
  (is equal '(x (1 2)) (match-one '(n () . more) '(x () 1 2)))
  (false (match-one '(n (m) . more) '(x () 1 2)))
  (is equal '(x 3 (1 2)) (match-one '(n (m) . more) '(x (3) 1 2))))

(define-test ("org.tfeb.dsm.bugs" "dotty")
  (is equal '(1 (2)) (match-one '(a &rest b) '(1 2)))
  (is equal '(1 (2)) (match-one '(a . b) '(1 2)))
  (is equal '(1 2) (match-one '(a . b) '(1 . 2))))

(define-test ("org.tfeb.dsm.bugs" "recursive")
  (is equal '(2) (match-one '((a)) '((2))))
  (false (match-one '((a)) '()))
  (is equal '(1 nil t t nil 2)
      (match-one '(a (&key (b nil bp) (c t cp)) d)
                 '(1 (:b nil) 2)))
  (is equal '(((n :b 3) (1 2 3) 4 5)
              n 3 t (1 2 3) (4 5))
      (match-one '(&whole w (n &key (b nil bp)) (&rest args) &body forms)
                 '((n :b 3) (1 2 3) 4 5)))
  (is equal '(x y) (match-one '(((a)) b) '(((x)) y))))

(when *test-individually*
  (test "org.tfeb.dsm.bugs" :report *test-report-class*))

