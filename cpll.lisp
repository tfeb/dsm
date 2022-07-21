;;;; Parsed lambda list compiler
;;;

(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.dsm/impl)

(needs ((:org.tfeb.hax.simple-loops
         :org.tfeb.hax.utilities
         :org.tfeb.hax.collecting)
        :compile t :use t)
       (:org.tfeb.hax.spam :compile t :use t)
       ("low" :compile t)
       ("pll" :compile t))

;;; Again, the first part of this file is in principle a
;;; general-purpose thing, but in pracice it's only ever been used for
;;; destructuring lambda lists.
;;;

(defvar *pll-compilers* (cons '() nil))

(defun clear-pll-compilers ()
  (setf *pll-compilers* (cons '() nil)))

(defun pll-compiler (for &optional (pll-compilers *pll-compilers*))
  (looping ((compilers pll-compilers))
    (if (null compilers)
        (return nil)
      (let ((found (assoc for (car compilers))))
        (if found
            (return (cdr found))
          (cdr compilers))))))

(defun (setf pll-compiler) (new for &optional (pll-compilers *pll-compilers*))
  ;; If the compiler is not bound anywhere, bind it in pll-compilers.
  ;; If it's bound already clobber that binding.  Yes, this conflates
  ;; binding and assignment and yes, that's fine.
  (looping ((compilers pll-compilers))
    (let ((found (assoc for (car compilers))))
      (cond
       (found
        (return (setf (cdr found) new)))
       ((null (cdr compilers))
        (push (cons for new) (car pll-compilers))
        (return new))
       (t
        (cdr compilers))))))

(declaim (inline fail))                 ;save a funcall
(defun fail (failure-continuation &optional (argument "failed") &rest args)
  (funcall failure-continuation argument args))

(defun canonicalize-declarations (declarations)
  ;; Turn DECLARATIONS into a larger set which affect at most one
  ;; variable each.  This is safe, but intentianally does not even try
  ;; to hack (declare (integer ...)) as there's no reliable way of
  ;; even knowing what is a type specifier.
  (collecting
    (dolist (decl declarations)
      (dolist (dspec (rest decl))
        (matching dspec
          ((head-matches (is 'type) (any))
           ;; obvious type declaration
           (destructuring-bind (tp . vars) (rest dspec)
             (dolist (v vars)
               (collect `(declare (type ,tp ,v))))))
          ((head-matches (some-of (is 'ignore)
                                  (is 'ignorable)
                                  (is 'special)
                                  (is 'dynamic-extent)))
           (destructuring-bind (d . vars) dspec
             (dolist (v vars)
               (collect `(declare (,d ,v))))))
          (otherwise
           ;; No idea
           (collect `(declare ,dspec))))))))

(defun declarations-for (variables canonical-declarations)
  ;; Return a list of declarations affecting VARs and the remaining
  ;; declarations.  Again this *won't* deal with (declare (integer
  ;; x)).
  (with-collectors (for-variables not-for-variables)
    (dolist (d canonical-declarations)
      (matching (second d)
        ((list-matches (some-of (is 'ignore)
                                (is 'ignorable)
                                (is 'special)
                                  (is 'dynamic-extent))
                       (all-of (var)
                               (lambda (v) (member v variables))))
         (for-variables d))
        ((list-matches (is 'type) (any) (all-of (var)
                                                (lambda (v) (member v variables))))
           (for-variables d))
          (otherwise
           (not-for-variables d))))))

(defun compile-parsed-lambda-list (pll variables anonymous decls/body &optional
                                       (pll-compilers *pll-compilers*))
  ;; compile a parsed lambda list, adding IGNORE declarations for
  ;; ANONYMOUS, and using DECLS/BODY as the body. VARS is not used currently
  (declare (ignore variables))
  (let ((<thing> (make-symbol "THING"))
        (<fail> (make-symbol "FAIL")))
    (multiple-value-bind (cdecls body)
        (multiple-value-bind (decls body)
            (parse-simple-body decls/body)
          (values (append (mapcar (lambda (anon)
                                    `(declare (ignore ,anon)))
                                  anonymous)
                          (canonicalize-declarations decls))
                  body))
      `(lambda (,<thing> ,<fail>)
         ,(iterate crl ((pllt pll) (rdecls cdecls) (next nil))
            (matching pllt
              (#'null
               ;; no more lambda list at this level
               `(if (not (null ,<thing>)) ;this is often redundant
                    (fail ,<fail> "extra arguments")
                  ,(if next
                       ;; recursive level: call continuation
                       (funcall next rdecls)
                     ;; we're really done
                     `(locally
                        ,@rdecls
                        ,@body))))
              ((head-matches
                (head-matches (all-of #'symbolp
                                      (none-of #'null))))
               ;; something like ((&required ...) ...).  Note we must
               ;; not match ((nil ...))
               (destructuring-bind ((key . clause-body) . more) pllt
                 (let ((compiler (pll-compiler key pll-compilers)))
                   (unless compiler
                     (scold "no compiler for ~S" key))
                   (funcall compiler
                            clause-body rdecls
                            (lambda (rds) (crl more rds next))
                            <thing> <fail>))))
              ((head-matches #'listp)
               ;; Recursive case: head it a list & its head is not a
               ;; non-null symbol (since we checked for that above)
               (destructuring-bind (recursive . more) pllt
                 (let ((<more> (make-symbol "MORE")))
                   ;; Each recursive case needs its own <more> name
                   ;; so it can find the right one.
                   `(if (consp ,<thing>)
                        (let ((,<thing> (first ,<thing>))
                              (,<more> (rest ,<thing>)))
                          (if (listp ,<thing>)
                              ,(crl recursive rdecls
                                    (lambda (rds)
                                      `(let ((,<thing> ,<more>))
                                         ,(crl more rds next))))
                            (fail ,<fail> "nested arguments aren't")))
                      (fail ,<fail> "nothing for nested arglist")))))
              (otherwise
               ;; Can't happen absent bugs in parse-lambda-list
               (catastrophe "what even is ~S? (from ~S)" pllt pll))))))))

(defun compile-lambda-list (ll decls/body &optional
                               (pll-compilers *pll-compilers*))
  ;; This just exists so destructuring-bind doesn't have to care about
  ;; variables &c
  (multiple-value-bind (pll variables anonymous) (parse-lambda-list ll)
    (compile-parsed-lambda-list pll variables anonymous decls/body pll-compilers)))

(defmacro define-pll-compiler (name/in (clause-body <thing> <fail>)
                                       &body decls/forms)
    (multiple-value-bind (name in)
        (matching name/in
          (#'symbolp
           (values name/in '*pll-compilers*))
          ((list-matches #'symbolp (any))
           (values (first name/in) (second name/in)))
          (otherwise
           (scold "bad pll compiler spec")))
      (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
        (let ((<next> (make-symbol "NEXT"))
              (<rdecls> (make-symbol "RDECLS")))
          `(progn
             (setf (pll-compiler ',name ,in)
                   (lambda (,clause-body ,<rdecls> ,<next> ,<thing> ,<fail>)
                     ,@decls
                     (block ,name
                       (flet ((decls/next (&rest var/s)
                                (multiple-value-bind (ours more)
                                    (declarations-for
                                     (mapcan (lambda (v/s)
                                               (etypecase v/s
                                                 (list (copy-list v/s))
                                                 (symbol (list v/s))))
                                             var/s)
                                     ,<rdecls>)
                                  `(,@ours ,(funcall ,<next> more)))))
                         ,@forms))))
             ',name)))))

;;; Compilers for destructuring lambda lists
;;;

(define-pll-compiler &whole (clause-body <thing> <fail>)
  (declare (ignore <fail>))
  (let ((v (first clause-body)))
    `(let ((,v ,<thing>))
       ,@(decls/next v))))

(define-pll-compiler &required (clause-body <thing> <fail>)
  (let ((v (first clause-body)))
    `(if (consp ,<thing>)
         (let ((,v (first ,<thing>))
               (,<thing> (rest ,<thing>)))
           ,@(decls/next v))
       (fail ,<fail> "nothing for required argument"))))

(define-pll-compiler &optional (clause-body <thing> <fail>)
  (declare (ignore <fail>))
  (destructuring-bind (v default suppliedp) clause-body
    (if suppliedp
        `(if (consp ,<thing>)
             (let ((,v (first ,<thing>))
                   (,suppliedp t)
                   (,<thing> (rest ,<thing>)))
               ,@(decls/next v suppliedp))
           (let ((,v ,default)
                 (,suppliedp nil))
             ,@(decls/next v suppliedp)))
      `(if (consp ,<thing>)
           (let ((,v (first ,<thing>))
                 (,<thing> (rest ,<thing>)))
             ,@(decls/next v))
         (let ((,v ,default))
           ,@(decls/next v))))))

(define-pll-compiler &rest (clause-body <thing> <fail>)
  (declare (ignore <fail>))
  (destructuring-bind (v keyp) clause-body
    (if keyp
        `(let ((,v ,<thing>))
           ,@(decls/next v))
      `(let ((,v ,<thing>)
             (,<thing> '()))
         ,@(decls/next v)))))

(define-pll-compiler &aux (clause-body <thing> <fail>)
  (declare (ignore <thing> <fail>))
  (destructuring-bind (v init) clause-body
    `(let ((,v ,init))
       ,@(decls/next v))))

(define-pll-compiler &keywords (clause-body <thing> <fail>)
  ;; This not likely to be efficient, but it works, I think
  (destructuring-bind (specs allow-other-keys) clause-body
    (multiple-value-bind (keywords variables temporaries initforms suppliedps)
        (with-collectors (k v tv i s)
          (dolist (spec specs)
               (destructuring-bind ((keyword variable) initform suppliedp) spec
                 (k keyword)
                 (v variable)
                 (tv (make-symbol (string variable)))
                 (i initform)
                 (s (or suppliedp (make-symbol (concatenate 'string
                                                            (symbol-name variable)
                                                            "P")))))))
      (let ((<allow-other-keys> (make-symbol "ALLOW-OTHER-KEYS")))
      `(let (,@(collecting
                 (dolist (tv temporaries) (collect `(,tv nil))))
             ,@(collecting
                 (dolist (s suppliedps) (collect `(,s nil))))
             ,@(if (not allow-other-keys)
                   `((,<allow-other-keys> nil))))
         (do ((,<thing> ,<thing> (cddr ,<thing>)))
             ((null ,<thing>)
              (let* ,(mapcar (lambda (variable temporary initform suppliedp)
                               `(,variable
                                 (if ,suppliedp ,temporary ,initform)))
                             variables temporaries initforms suppliedps)
                ,@(decls/next variables suppliedps)))
           (cond
            ((not (consp ,<thing>))
             (fail ,<fail> "arguments aren't a proper list"))
            ((null (rest ,<thing>))
             (fail ,<fail> "odd number of keyword arguments")))
           (case (first ,<thing>)
             ,@(if (not allow-other-keys)
                   `(((:allow-other-keys)
                      (setf ,<allow-other-keys> (second ,<thing>))))
                 ())
             ,@(mapcar (lambda (keyword temporary suppliedp)
                         `((,keyword)
                           (unless ,suppliedp
                             (setf ,temporary (second ,<thing>)
                                   ,suppliedp t))))
                       keywords temporaries suppliedps)
             (otherwise
              ,(if allow-other-keys
                   'nil
                 `(unless ,<allow-other-keys>
                    ;; We have to trawl down the arguments.  GETF is
                    ;; unsafe here.
                    (do ((tail ,<thing> (cddr tail)))
                        ((null tail))
                      (cond
                       ((not (consp tail))
                        (fail ,<fail> "arguments aren't a proper list"))
                       ((null (rest tail))
                        (fail ,<fail> "odd number of keyword arguments"))
                       ((eql (first tail) ':allow-other-keys)
                        (setf ,<allow-other-keys> (second tail)))))
                    (unless ,<allow-other-keys>
                      (fail ,<fail> "unexpected keyword"))))))))))))
