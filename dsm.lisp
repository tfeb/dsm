;;;; Destructuring match
;;;

(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.dsm)

(needs ((:org.tfeb.hax.collecting
         :org.tfeb.hax.utilities
         :org.tfeb.hax.simple-loops
         :org.tfeb.hax.spam)
        :compile t)
       ("cpll" :compile t))

(provides :org.tfeb.dsm)

(defun compute-guard (clauses)
  (let ((effective
         (collecting
           (looping ((ctail clauses))
             (destructuring-bind (key clause . more) ctail
               (case key
                 ((:when)
                  (collect clause))
                 ((:unless)
                  (collect `(not ,clause)))
                 (otherwise
                  (scold "guards must be :when ... or :unless ... so far")))
               (if (null more)
                   (return)
                 more))))))
    (if (= (length effective) 1)
        (first effective)
      `(and ,@effective))))

(defmacro destructuring-match (form &body clauses)
  (let ((<form> (make-symbol "FORM"))
        (<done> (make-symbol "DONE")))
    `(let ((,<form> ,form))
       (block ,<done>
         (let ((,<done> (lambda (&rest values)
                          (declare (dynamic-extent values))
                          (return-from ,<done> (values-list values)))))
           (tagbody
            ,@(collecting
                (dolist (clause clauses)
                  (matching clause
                    ((head-matches (some-of (is 'otherwise)
                                            (is 't)))
                     (collect
                      `(multiple-value-call ,<done>
                         (locally ,@(rest clause)))))
                    ((head-matches
                      (any)
                      (all-of #'consp   ;at least one guard clause
                              (repeating-list-of #'keywordp (any))))
                     (destructuring-bind (ll (&rest guard-clauses
                                                    &key &allow-other-keys)
                                             &body decls/body) clause
                       (multiple-value-bind (decls body)
                           (parse-simple-body decls/body)
                         (let ((<next> (make-symbol "NEXT"))
                               (guard (compute-guard guard-clauses)))
                           (collect
                            `(funcall ,(compile-lambda-list
                                        ll
                                        `(,@decls
                                          (if ,guard
                                              (multiple-value-call ,<done>
                                                (progn ,@body))
                                            (go ,<next>))))
                                      ,<form>
                                      (lambda (message arguments)
                                        (declare (ignore message arguments))
                                        (go ,<next>))))
                           (collect <next>)))))
                    ((head-matches (head-matches (any)))
                     (destructuring-bind (ll &body decls/body) clause
                       (multiple-value-bind (decls body)
                           (parse-simple-body decls/body)
                         (let ((<next> (make-symbol "NEXT")))
                           (collect
                            `(funcall ,(compile-lambda-list
                                        ll
                                        `(,@decls
                                          (multiple-value-call ,<done>
                                            (progn ,@body))))
                                      ,<form>
                                      (lambda (message arguments)
                                        (declare (ignore message arguments))
                                        (go ,<next>))))
                           (collect <next>))))))))))))))

#||
(destructuring-match '(1 :x 2)
 ((y &key x)
  (:when (symbolp y))
  y)
 ((y &key (x 3))
  (:when (not (symbolp y)))
  x))
||#
