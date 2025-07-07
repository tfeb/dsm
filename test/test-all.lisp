;;;; Run the tests unless testing individually
;;;

(in-package :org.tfeb.dsm/test)

(unless *test-individually*
  (let ((result (test "org.tfeb.dsm" :report *test-report-class*)))
    (when (eq (status result) ':failed)
      (error "Tests failed" result))
    result))
