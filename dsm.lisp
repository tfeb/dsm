;;;; Destructuring match
;;;

#+org.tfeb.tools.require-module
(progn
  (org.tfeb.tools.require-module:needs
   ((:org.tfeb.hax.collecting
     :org.tfeb.hax.utilities
     :org.tfeb.hax.simple-loops
     :org.tfeb.hax.spam)
    :compile t)
   (("pkg" "cpll") :compile t))
  (org.tfeb.tools.require-module:provides :org.tfeb.dsm))

#-org.tfeb.tools.require-module
(provide :org.tfeb.dsm)

(in-package :org.tfeb.dsm)

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
  "Match FORM against CLAUSES, returning the values of the matching clause

Each clause in clauses is ({<dsm-ll> | OTHERWISE | T} [<guard>] . <decls/body>).

<dsm-ll> is either a lambda list suitable for DESTRUCTURING-BIND,
except that any non-keyword variables whose name is _ are blanks which
are ignored, and each blank is different, or a variable symbol which
binds the whole FORM.  OTHERWISE and T are default clauses.

<guard> is of the form ({:when | :unless} <expression> ...) and, if
present, causes the expressions to be called, with variables bound, to
decide if the clause matched."
  (let ((<form> (make-symbol "FORM"))
        (<done> (make-symbol "DONE")))
    `(let ((,<form> ,form))
       (block ,<done>
         (let ((,<done> (lambda (&rest values)
                          (declare (dynamic-extent values))
                          (return-from ,<done> (values-list values)))))
           (tagbody
            ,@(collecting
                (do* ((ctail clauses (rest ctail))
                      (clause (first ctail) (first ctail)))
                     ((not (consp ctail)))
                  (matching clause
                    ((head-matches (some-of (is 'otherwise)
                                            (is 't)))
                     (unless (null (rest ctail))
                       (scold "default clause is not last in ~S" clauses))
                     (collect
                      `(multiple-value-call ,<done>
                         (locally ,@(rest clause)))))
                    ((head-matches
                      (some-of #'listp (var)) ;remember variables are lls
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
                    ((head-matches (some-of #'listp (var)))
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
                           (collect <next>)))))
                    (otherwise
                     (scold "clause ~S isn't" clause)))))))))))

#||
(destructuring-match '(1 :x 2)
 ((y &key x)
  (:when (symbolp y))
  y)
 ((y &key (x 3))
  (:when (not (symbolp y)))
  x))
||#
