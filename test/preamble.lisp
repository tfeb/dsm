;;;; Preamble to tests
;;;

(in-package :org.tfeb.dsm/test)

(defvar *test-individually* nil)

(defvar *test-report-class* 'summary)

(define-test "org.tfeb.dsm")

(defun match-one (lambda-list form &key (print-compiled nil)
                              (bindings '() bindingsp))
  ;; Does FORM match LAMBDA-LIST? Return either a list of argument
  ;; values and NIL, or NIL and a string describing something wrong.
  ;; If BINDINGS is given wrap this around the call.  On any kind of
  ;; compilation error or warning signal a catastrophe.
  (multiple-value-bind (pll variables anonymous) (parse-lambda-list lambda-list)
    (let* ((warnings '())
           (cpl (if bindingsp
                    (let ((<thing> (make-symbol "THING"))
                          (<fail> (make-symbol "FAIL")))
                      `(lambda (,<thing> ,<fail>)
                         (let ,bindings
                           (,(compile-parsed-lambda-list
                              pll variables anonymous
                              `((list ,@variables)))
                            ,<thing> ,<fail>))))
                  (compile-parsed-lambda-list
                   pll variables anonymous
                   `((list ,@variables)))))
           (f (handler-bind ((warning
                              (lambda (w)
                                (push w warnings))))
                (compile nil cpl))))
    (when print-compiled
      (pprint cpl))
    (when warnings
      (catastrophe "compilation warnings:~{~% ~A~}~%"
                   (reverse warnings)))
    (block fail
      (let ((values (funcall f form
                             (lambda (message arguments)
                               (return-from fail
                                 (values
                                  nil
                                  (apply #'format nil message arguments)))))))
        (unless (equal (length variables) (length values))
          (catastrophe "binding botch: ~D / ~D"
                       (length variables) (length values)))
        (values values nil))))))

(defun vars/anons (ll)
  (multiple-value-bind (parsed vars anons) (parse-lambda-list ll)
    (declare (ignore parsed))
    (values vars anons)))

(define-test ("org.tfeb.dsm" "match-one")
  (is-values (match-one '(x) '(1))
    (equal '(1))
    (eql nil))
  (isnt-values (match-one '(x) '())
    (equal '(1))
    (eql nil))
  (fail (match-one '(&key &key) '())
        scold))
