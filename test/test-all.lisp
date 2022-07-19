;;;; Run the tests unless testing individually
;;;

(in-package :org.tfeb.dsm/test)

(unless *test-individually*
  (test "org.tfeb.dsm" :report *test-report-class*))
