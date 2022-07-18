;;;; Test lambda list interpretation for dsm
;;;
;;; Note that 'lambda list' means 'destructuring-match lambda list'.
;;; These should be the same as destructuring-match lambda lists with
;;; the exception that 'lambda lists' which are symbols work like
;;; &rest (this was an accident, but its a nice one).
;;;
;;; This is not complete by any means, but it does test a bunch of
;;; useful cases.
;;;

(in-package :org.tfeb.dsm/test)

(define-test ("org.tfeb.dsm" "org.tfeb.dsm.ll"))

(define-test ("org.tfeb.dsm.ll" "required")
  (is equal '(1) (match-one '(a) '(1)))
  (false (match-one '(a) '()))
  (false (match-one '(a) '(1 2)))
  (let ((ll '(a b c)))
    (is equal '(1 2 3) (match-one ll '(1 2 3)))
    (false (match-one ll '(1 2)))
    (false (match-one ll '(1 2 3 4)))))

(define-test ("org.tfeb.dsm.ll" "simple-errors")
  (fail (match-one '(a a) '()))
  (fail (match-one '(a b a) '())))
  
(define-test ("org.tfeb.dsm.ll" "optional")
  (is equal '(1 nil) (match-one '(a &optional b) '(1)))
  (is equal '(1 2) (match-one '(a &optional b) '(1 2)))
  (false (match-one '(a &optional b) '(1 2 3)))
  (is equal '(1 nil) (match-one '(&optional (a 1 ap)) '()))
  (is equal '(2 t) (match-one '(&optional (a 1 ap)) '(2))))

(define-test ("org.tfeb.dsm.ll" "rest")
  (is equal '(()) (match-one '(&rest x) '()))
  (is equal '((1)) (match-one '(&rest x) '(1)))
  (is equal '(1 ()) (match-one '(a &rest b) '(1)))
  (is equal '(1 (2 3)) (match-one '(a &rest b) '(1 2 3)))
  (let ((orl '(a &optional (b nil bp) &rest c)))
    (is equal '(1 nil nil ()) (match-one orl '(1)))
    (is equal '(1 nil t ()) (match-one orl '(1 nil)))
    (is equal '(1 2 t (3)) (match-one orl '(1 2 3))))
  (fail (match-one '(&rest a &optional b) '()))
  (finish (match-one '(&optional a &rest b) '()))
  (fail (match-one '(&optional a &rest (b bp)) '())))

(define-test ("org.tfeb.dsm.ll" "dotty")
  (is equal '(1 (2 3)) (match-one '(x . y) '(1 2 3)))
  (let ((oll '(x &optional (y nil yp) . z)))
    (is equal '(1 nil nil ()) (match-one oll '(1)))
    (is equal '(1 2 t ()) (match-one oll '(1 2)))
    (is equal '(1 2 t (3)) (match-one oll '(1 2 3))))
  (fail (match-one '(x &key y . z) '(1 :y 3 4)))
  (fail (match-one '(x &rest y . z) '(1 2 3))))

(define-test ("org.tfeb.dsm.ll" "symbol")
  ;; This is dsm-specific
  (is equal '((1 2 3)) (match-one 'x '(1 2 3))))

(define-test ("org.tfeb.dsm.ll" "whole")
  (is equal '(()) (match-one '(&whole a) '()))
  (false (match-one '(&whole a) '(1)))
  (is equal '((1) 1) (match-one '(&whole a b) '(1)))
  (is equal '((1 2 3) 1 (2 3))
      (match-one '(&whole a b &rest c) '(1 2 3))))

(define-test ("org.tfeb.dsm.ll" "simple-keywords")
  (is equal '(1) (match-one '(&key x) '(:x 1)))
  (is equal '(1 nil) (match-one '(&key (x 1 xp)) '()))
  (is equal '(1 t) (match-one '(&key (x 1 xp)) '(:x 1)))
  (is equal '(1 nil) (match-one '(&key ((:x y) 1 yp)) '()))
  (is equal '(1 t) (match-one '(&key ((:x y) 1 yp)) '(:x 1)))
  (false (match-one '(&key x) '(:x)))
  (false (match-one '(&key x) '(:x 1 :y)))
  (false (match-one '(&key x) '(:x 1 2)))
  (is equal '(1 1) (match-one '(&key y (z y)) '(:y 1 :y 2))))

(define-test ("org.tfeb.dsm.ll" "keyword-defaults")
  (is equal '(nil nil) (match-one '(&key x (y x)) '()))
  (is equal '(1 1) (match-one '(&key x (y x)) '(:x 1)))
  (is equal '(1 nil 1 nil) (match-one '(&key (x 1 xp) (y x yp)) '()))
  (is equal '(1 t 1 nil) (match-one '(&key (x 1 xp) (y x yp)) '(:x 1)))
  (is equal '(1 nil 2 t) (match-one '(&key (x 1 xp) (y x yp)) '(:y 2)))
  (is equal '(3 4) (match-one '(&key (x y) (y 4)) '()
                                :bindings '((y 3))))
  (is equal '(3 nil) (match-one '(&key (y y yp)) '()
                                :bindings '((y 3)))))

(define-test ("org.tfeb.dsm.ll" "keywords-and-others")
  (is equal '(1 2 t) (match-one '(x &key (y nil yp)) '(1 :y 2)))
  (is equal '((1 :y 2) 1 2 t) (match-one '(&whole w x &key (y nil yp))
                                         '(1 :y 2)))
  (let ((okll '(&optional x &key y)))
    (is equal '(nil nil) (match-one okll '()))
    (is equal '(:y nil) (match-one okll '(:y)))
    (is equal '(:y 1) (match-one okll '(:y :y 1)))
    (false (match-one okll '(1 :y)))
    (false (match-one okll '(1 :y 2 3))))
  (is equal '((:x 1) 1) (match-one '(&rest r &key x) '(:x 1))))

(define-test ("org.tfeb.dsm.ll" "allow-other-keys")
  (is equal '(1) (match-one '(&key x &allow-other-keys)
                            '(:y 2 :x 1)))
  (is equal '(1) (match-one '(&key x)
                            '(:x 1 :allow-other-keys t
                              :y 2)))
  (false (match-one '(&key x)
                    '(:x 1 :allow-other-keys nil
                      :y 2)))
  (false (match-one '(&key x)
                            '(:x 1 :allow-other-keys t
                              :y))))

(define-test ("org.tfeb.dsm" "recursive")
  (is equal '(1 2) (match-one '(x () y) '(1 () 2)))
  (is equal '(1 2 3) (match-one '(x (y) z) '(1 (2) 3)))
  (false (match-one '(x (y) z) '(1 () 2)))
  (false (match-one '(x (y) z) '(1 (2 3) 4)))
  (is equal '(1 (2 3) (4 5)) (match-one '((y . z) . a) '((1 2 3) 4 5)))
  (let ((rkll '(x (&key (y nil yp)) &key (z nil zp))))
    (is equal '(1 2 t 3 t) (match-one rkll '(1 (:y 2) :z 3)))
    (fail (match-one rkll '(1 (:y 2 :z 3) :z 4)))
    (fail (match-one rkll '(1 (:y 2) :z 4 :y 4)))
    (is equal '(1 2 t 3 t)
        (match-one rkll '(1 (:y 2 :k 4 :allow-other-keys t) :z 3)))
    (fail (match-one rkll '(1 (:y 2 :k 4 :allow-other-keys t) :z 3 :k 4)))))

(define-test ("org.tfeb.dsm" "aux")
  (is equal '(1 2) (match-one '(x &aux (y 2)) '(1)))
  (is equal '(1 1) (match-one '(x &aux (y x)) '(1))))

(test "org.tfeb.dsm.ll" :report 'summary)
