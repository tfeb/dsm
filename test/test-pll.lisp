;;;; Some tests on parsing lambda lists
;;;
;;; It is slightly hard to know what to test here
;;;

(in-package :org.tfeb.dsm/test)

(define-test ("org.tfeb.dsm" "org.tfeb.dsm.pll"))

(define-test ("org.tfeb.dsm.pll" "sanity")
  (finish (parse-lambda-list '(x y z)))
  (fail (parse-lambda-list '(x y z x)) scold)
  (finish (parse-lambda-list '(_ _ _))))

(define-test ("org.tfeb.dsm.pll" "questionable")
  (fail (parse-lambda-list '(:x)))      ;this is right
  (fail (parse-lambda-list '(:_))))     ;but is this?

(define-test ("org.tfeb.dsm.pll" "anonymous")
  (multiple-value-bind (vars anons) (vars/anons '(x y _ _ _ z))
    (is = 6 (length vars))
    (is = 3 (length anons))
    (is = 3 (length (remove-duplicates anons)))
    (true (every (lambda (anon)
                   (string= (symbol-name anon) "_"))
                 anons))
    (false (member '_ anons))))

(when *test-individually*
  (test "org.tfeb.dsm.pll" :report *test-report-class*))
