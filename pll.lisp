;;;; Lambda list recognizer & parser
;;;
;;; This is much more complex than it needs to be, but, well.
;;;
;;; Note that priorities are not now used and could go.
;;;

(org.tfeb.tools.require-module:needs
 ("pkg" :compile t))

(in-package :org.tfeb.dsm/impl)

(needs ((:org.tfeb.hax.collecting
         :org.tfeb.hax.iterate
         :org.tfeb.hax.simple-loops
         :org.tfeb.hax.utilities
         :org.tfeb.hax.spam)
        :compile t)
       ("low" :compile t))

;;; In principle the first part of this file is general-purpose: you
;;; could use it by binding *states*, *initial-state*, *final-state*
;;; and *state-recognizers* to recognize other kinds of lambda lists.
;;; In practice it is too entangled with the second half to be useful
;;; on its own.
;;;

(defvar *states* '())

(defvar *initial-state* nil)

(defvar *final-state* nil)

(defmacro define-states (&body states)
  `(progn
     ,@(mapcar (lambda (state)
                 `(defconstant ,state ',state))
               states)
     (dolist (state ',states)
       (pushnew state *states*))
     ',states))

(defun statep (s)
  (member s *states*))

(defvar *state-recognizers*
  ;; a cons of an alist of (from . recognizers) and either NIL or
  ;; parent state recognizers.  recognizers is a list of (name
  ;; priority recognizer), where names are unique
  (cons '() nil))

(defun make-state-recognizers (&optional (parent *state-recognizers*))
  (cons '() parent))

(defun clear-state-recognizers (&optional (and-states nil))
  (setf *state-recognizers* (make-state-recognizers nil))
  (when and-states
    (setf *states* '()
          *initial-state* nil
          *final-state* nil))
  nil)

(defun install-state-recognizer (name from/s priority recognizer &optional
                                     (state-recognizers *state-recognizers*))
  ;; Install a state recogniser called NAME for one of more from
  ;; states FROM/S with priority PRIORITY into STATE-RECOGNIZERS.
  (cond
   ((statep from/s)
    (let ((recognizers (assoc from/s (car state-recognizers))))
      (if recognizers
          (let ((found (assoc name (cdr recognizers))))
            (if found
                (progn
                  (setf (third found) recognizer)
                  (unless (= priority (second found))
                    (setf (second found) priority)
                    (setf (cdr recognizers) (sort (cdr recognizers) #'>
                                                  :key #'second))))
              (setf (cdr recognizers) (merge 'list `((,name ,priority ,recognizer))
                                             (cdr recognizers)
                                             #'>
                                             :key #'second))))
        (push `(,from/s . ((,name ,priority ,recognizer)))
              (car state-recognizers)))))
   ((listp from/s)
    (dolist (from from/s)
      (install-state-recognizer name from priority recognizer state-recognizers)))
    (t
     (scold "bad from state ~S" from/s)))
  recognizer)

(defun recognize (state ll collector
                        &optional (state-recognizers *state-recognizers*))
  ;; find a recognizer from STATE which accepts LL, and return the new
  ;; state, the new lambda list, and a recursive state, or NIL.
  ;; States are never NIL: NIL indicates failure or no recursive state
  (looping ((sr state-recognizers))
    (if (null sr)
        (return-from recognize (values nil nil nil))
      (destructuring-bind (this . next) sr
        (let ((found (assoc state this)))
          (if found
              (dolist (d (cdr found) next)
                (multiple-value-bind (new-state new-ll recursive-state)
                    (funcall (third d) ll collector)
                  (when new-state
                    (return-from recognize (values new-state new-ll
                                                   recursive-state)))))
            next))))))

(defun make-state-recognizer (predicate function)
  ;; A recognizer is a function which takes two arguments: a lambda
  ;; list and a collector, and which if it recognizes something in the
  ;; lambda list, returns the result of calling its function on the
  ;; lambda list and collector, which should be a new state, a tail of
  ;; the lambda list, and an indication of recursion.
  (lambda (ll collector)
    (if (funcall predicate ll)
        (funcall function ll collector)
      (values nil nil nil))))

(defmacro define-state-recognizer ((name from/s predicate &key
                                         (priority 0)
                                         (state-recognizers '*state-recognizers*))
                                   (ll collector)
                                   &body decls/forms)
  ;; Define a state recognizer named NAME for state/s FROM/S which
  ;; matches if PREDICATE is true
  (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
    `(progn
       (install-state-recognizer
             ',name ',from/s ,priority
             (make-state-recognizer ,predicate
                                    (lambda (,ll ,collector)
                                      ,@decls
                                      (block ,name ,@forms)))

             ,state-recognizers)
       ',name)))

;;; Recognizing lambda lists for destrucuring-bind
;;;
;;; From CLHS, with fixes
;;;
;;; lambda-list   ::= (wholevar reqvars optvars restvar keyvars auxvars) |
;;;                   (wholevar reqvars optvars . var)
;;; reqvars       ::= {var|lambda-list}*
;;;

;;; It would be a good thing if the structures this built were so
;;; explicitly just lists and there was generally less list access
;;; (see the keyword coalescing stuff for particularly nasty examples.
;;; Lists are fine, but there should be accessors &c (like InterLisp's
;;; stuff for defining names for getting at bits of list structure,
;;; which I forget how it worked).
;;;

(define-states
  <ds-lambda-list>
  <required>
  <optionals>
  <end-of-rest>
  <keywords>
  <end-of-keywords>
  <auxvars>
  <end>)

(setf *initial-state* <ds-lambda-list>
      *final-state* <end>)

(defun ->keyword (thing)
  (values (intern (string thing)
                  (load-time-value (find-package "KEYWORD")))))

(define-state-recognizer (whole <ds-lambda-list>
                                (head-matches (is '&whole) #'symbolp))
    (ll collector)
  (destructuring-bind (v . more) (rest ll)
    (collect-into collector `(&whole ,v))
    (values '<required> more nil)))

(define-state-recognizer (recursive (<ds-lambda-list> <required>)
                                    (head-matches #'listp)
                                    :priority 1)
    (ll collector)
  (declare (ignore collector))
  (values '<required> (rest ll) '<ds-lambda-list>))

(define-state-recognizer (required (<ds-lambda-list> <required>)
                                   (head-matches (var)))
    (ll collector)
  (destructuring-bind (v . more) ll
    (collect-into collector `(&required ,v))
    (values <required> more nil)))

(define-state-recognizer (->optionals (<required> <ds-lambda-list>)
                                      (head-matches (is '&optional)))
    (ll collector)
  (declare (ignore collector))
  (values <optionals> (rest ll) nil))

(define-state-recognizer (simple-optional <optionals>
                                          (head-matches (var)))
    (ll collector)
  (destructuring-bind (v . more) ll
    (collect-into collector `(&optional ,v nil nil))
    (values <optionals> more nil)))

(define-state-recognizer (listy-optional <optionals>
                                         (head-matches (list-matches (var))))
    (ll collector)
  (destructuring-bind ((v) . more) ll
    (collect-into collector `(&optional ,v nil nil))
    (values <optionals> more nil)))

(define-state-recognizer (optional-with-default <optionals>
                                                (head-matches
                                                 (list-matches (var) (any))))
    (ll collector)
  (destructuring-bind ((v  initform) . more) ll
    (collect-into collector `(&optional ,v ,initform nil))
    (values <optionals> more nil)))

(define-state-recognizer (optional-with-supplied <optionals>
                                                  (head-matches
                                                   (list-matches
                                                    (var) (any) (var))))
    (ll collector)
  (destructuring-bind ((v  initform vp) . more) ll
    (collect-into collector `(&optional ,v ,initform ,vp))
    (values <optionals> more nil)))

(define-state-recognizer (tail-variable (<required> <ds-lambda-list> <optionals>)
                                        (var))
    (ll collector)
  ;; This is the recognizer which licenses symbols as lambda lists.
  ;; The 'fix' would be to have an additional initial state which
  ;; checks the lambda list is a cons or null before anything else.
  (collect-into collector `(&rest ,ll nil))
  (values <end> nil nil))

(define-state-recognizer (rest-variable (<required> <ds-lambda-list> <optionals>)
                                        (head-matches
                                         (some-of (is '&rest) (is '&body))
                                         (var)))
    (ll collector)
  (destructuring-bind (v . more) (rest ll)
    (matching ll
      ((head-matches (some-of (is '&rest) (is '&body))
                     (var)
                     (is '&key))
       (collect-into collector `(&rest ,v '&key)))
      (otherwise
       (collect-into collector `(&rest ,v nil))))
    (values <end-of-rest> more nil)))

(define-state-recognizer (->keywords (<ds-lambda-list> <required> <optionals>
                                                    <end-of-rest>)
                                     (head-matches (is '&key)))
    (ll collector)
  (declare (ignore collector))
  (values <keywords> (rest ll) nil))

(define-state-recognizer (simple-keyword <keywords> (head-matches (var)))
    (ll collector)
  (destructuring-bind (v . more) ll
    (collect-into collector `(&key (,(->keyword v) ,v)  nil nil))
    (values <keywords> more nil)))

(define-state-recognizer (listy-keyword <keywords>
                                        (head-matches
                                         (list-matches
                                          (some-of (var)
                                                   (list-matches
                                                    (some-of (var) #'keywordp)
                                                    (var))))))
    (ll collector)
  (destructuring-bind ((ks) . more) ll
    (multiple-value-bind (k v)
        (matching ks
          ((var) (values (->keyword ks) ks))
          ((list-matches (some-of (var) #'keywordp) (var))
           (values (first ks) (second ks)))
          (otherwise
           (catastrophe "can't happen")))
       (collect-into collector `(&key (,k ,v) nil nil)))
    (values <keywords> more nil)))

(define-state-recognizer (keyword-with-default <keywords>
                                               (head-matches
                                                (list-matches
                                                 (some-of (var)
                                                          (list-matches
                                                           (some-of (var) #'keywordp)
                                                           (var)))
                                                 (any))))
    (ll collector)
  (destructuring-bind ((ks  initform) . more) ll
    (multiple-value-bind (k v)
        (matching ks
          ((var) (values (->keyword ks) ks))
          ((list-matches (some-of (var) #'keywordp) (var))
           (values (first ks) (second ks)))
          (otherwise
           (catastrophe "can't happen")))
      (collect-into collector `(&key (,k ,v) ,initform nil)))
    (values <keywords> more nil)))

(define-state-recognizer (keyword-with-supplied <keywords>
                                                (head-matches
                                                 (list-matches
                                                  (some-of (var)
                                                           (list-matches
                                                            (some-of (var) #'keywordp)
                                                            (var)))
                                                  (any) (var))))
    (ll collector)
  (destructuring-bind ((ks  initform vp) . more) ll
    (multiple-value-bind (k v)
        (matching ks
          ((var) (values (->keyword ks) ks))
          ((list-matches (some-of (var) #'keywordp) (var))
           (values (first ks) (second ks)))
          (otherwise
           (catastrophe "can't happen")))
      (collect-into collector `(&key (,k ,v) ,initform ,vp)))
    (values <keywords> more nil)))

(define-state-recognizer (allow-other-keys <keywords>
                                           (head-matches (is '&allow-other-keys))) ;
    (ll collector)
  (collect-into collector '(&allow-other-keys))
  (values <end-of-keywords> (rest ll) nil))

(define-state-recognizer (->auxvars (<ds-lambda-list> <required> <optionals>
                                                   <keywords> <end-of-keywords>)
                                    (head-matches (is '&aux)))
    (ll collector)
  (declare (ignore collector))
  (values <auxvars> (rest ll) nil))

(define-state-recognizer (simple-auxvar <auxvars> (head-matches (var)))
    (ll collector)
  (destructuring-bind (v . more) ll
    (collect-into collector `(&aux ,v nil))
    (values <auxvars> more nil)))

(define-state-recognizer (auxvar-with-default <auxvars>
                                              (head-matches
                                               (list-matches (var) (any))))
    (ll collector)
  (destructuring-bind ((v init) . more) ll
    (collect-into collector `(&aux ,v ,init))
    (values <auxvars> more nil)))

(define-state-recognizer (end (<ds-lambda-list> <required> <optionals>
                                             <end-of-rest>
                                             <keywords> <end-of-keywords>
                                             <auxvars>)
                              #'null)
    (ll collector)
  (declare (ignore ll collector))
  (values <end> nil nil))

(defun recognize-lambda-list (lambda-list &key
                                          (initial-state *initial-state*)
                                          (final-state *final-state*)
                                          (state-recognizers *state-recognizers*)
                                          (trace nil))
  (unless (statep initial-state)
    (scold "not a state: ~S" initial-state))
  (when trace
    (format *trace-output* "~&from ~S to ~S with ~:S~%"
            initial-state final-state lambda-list))
  (iterate rll ((ll lambda-list)
                (start-state initial-state))
    (let ((collector (make-collector)))
      (looping ((state start-state)
                (llt ll))
        (unless (statep state)
          (scold "not a state: ~S" state))
        (when trace
          (format *trace-output* "~&~S with ~:S" state llt))
        (multiple-value-bind (new-state new-llt recursive-state)
            (recognize state llt collector state-recognizers)
          (when trace
            (format *trace-output* " -> ~S with ~:S~@[~% recursive state ~S~]~%"
                    new-state new-llt recursive-state))
          (cond
           ((not new-state)
            (scold "parse of ~S failed at ~S (old state ~S, no new state)"
                   lambda-list llt state))
           ((eq new-state final-state)
            (return (collector-contents collector)))
           (recursive-state
            (collect-into collector (rll (first llt) recursive-state))
            (values new-state new-llt))
           (t
            (values new-state new-llt))))))))

(defvar *anonymous-variable-names* '("_")) ;could expose this

(defun anonymize-variables (recognized-lambda-list &optional
                                                   (anonymous-variable-names
                                                    *anonymous-variable-names*))
  ;; Return a copy of RECOGNIZED-LAMBDA-LIST with anonymous variables
  ;; anonymized, and a list of the anonymized variables.  This shares
  ;; too much code with the next function.
  (let ((anonymous (make-collector)))
    (flet ((anonymize (v)
             (if (member (symbol-name v) anonymous-variable-names
                         :test #'string=)
                 (let ((va (make-symbol (symbol-name v))))
                   (collect-into anonymous va)
                   va)
               v)))
      (values
       (iterate rla ((rll recognized-lambda-list))
         (mapcar
          (lambda (this)
            (matching this
              ((head-matches #'symbolp)
               (case (first this)
                 ((&whole &required &rest &aux)
                  (destructuring-bind (llk v . more) this
                    `(,llk ,(anonymize v) ,@more)))
                 ((&optional)
                  (destructuring-bind (v i vp) (rest this)
                    `(&optional ,(anonymize v) ,i ,(anonymize vp))))
                 ((&key)
                  (destructuring-bind ((k v) i vp) (rest this)
                    `(&key (,k ,(anonymize v)) ,i ,(anonymize vp))))
                 ((&allow-other-keys)
                  this)
                 (otherwise
                  (catastrophe "mutant recognized lambda list ~S at ~S"
                               recognized-lambda-list this))))
              (#'listp
               (rla this))
              (otherwise
               (catastrophe "what even is ~S?" this))))
          rll))
       (collector-contents anonymous)))))

(defun unique-variables (recognized-lambda-list)
  ;; return a list of variables and a list of duplicates.  It is
  ;; annoying this shares so much code with the previous function.
  (with-collectors (variable duplicate)
    (iterate rll ((rlt recognized-lambda-list)
                  (variables '()))
      (unless (null rlt)
        (destructuring-bind (this . more) rlt
          (matching this
            ((head-matches #'symbolp)
             (case (first this)
               ((&whole &required &rest &aux)
                (let ((v (second this)))
                  (if (member v variables)
                      (duplicate v)
                    (variable v))
                  (rll more (cons v variables))))
               ((&optional)
                (destructuring-bind (v i vp) (rest this)
                  (declare (ignore i))
                  (if (member v variables)
                      (duplicate v)
                    (variable v))
                  (when vp
                    (if (or (eql vp v)
                            (member vp variables))
                        (duplicate vp)
                      (variable vp)))
                  (rll more
                       (if vp (list* vp v variables) (cons v variables)))))
               ((&key)
                (destructuring-bind ((k v) i vp) (rest this)
                  (declare (ignore k i))
                  (if (member v variables)
                      (duplicate v)
                    (variable v))
                  (when vp
                    (if (or (eql vp v)
                            (member vp variables))
                        (duplicate vp)
                      (variable vp)))
                  (rll more
                       (if vp (list* vp v variables) (cons v variables)))))
               ((&allow-other-keys)
                (rll more variables))
               (otherwise
                (catastrophe "mutant recognized lambda list ~S at ~S"
                             recognized-lambda-list this))))
            (#'listp
             (rll more (rll this variables)))
            (otherwise
             (catastrophe "what even is ~S?" rlt))))))))

(defun validate-keywords (recognized-lambda-list)
  ;; Check there is no :allow-other-keys anywhere
  (let ((problems (collecting
                    (iterate vkl ((rll recognized-lambda-list))
                      (dolist (e rll)
                        (matching e
                          ((head-matches (is '&key)
                                         (head-matches (is ':allow-other-keys)))
                           (collect e))
                          ((head-matches #'listp)
                           (vkl e))))))))
    (if (not (null problems))
        (values nil problems)
      (values t nil))))

(defun coalesce-keywords (recognized-lambda-list)
  ;; Coalesce multiple (&key ...) forms in a recognized lambda list
  ;; into a single `(&keywords ...) form which also says if other
  ;; keywords are allowed.
  (collecting
    (looping ((rlt recognized-lambda-list))
      (if (null rlt)
          (return)
        (matching rlt
          ((head-matches (head-matches (is '&key)))
           (let* ((ks (make-collector))
                  (ktt
                   (looping ((kt rlt))
                     (matching kt
                       ((head-matches (head-matches (is '&key)))
                        (collect-into ks (rest (first kt)))
                        (rest kt))
                       (otherwise (return kt))))))
             (matching ktt
               ((head-matches (list-matches (is '&allow-other-keys)))
                (collect `(&keywords ,(collector-contents ks)
                                     &allow-other-keys))
                (rest ktt))
               (otherwise
                (collect `(&keywords ,(collector-contents ks)
                                     nil))
                ktt))))
          ((head-matches (list-matches (is '&allow-other-keys)))
           (collect '(&keywords () &allow-other-keys))
           (rest rlt))
          ((head-matches (head-matches #'listp))
           ;; Recursive ll
           (collect (coalesce-keywords (first rlt)))
           (rest rlt))
          (otherwise
           (collect (first rlt))
           (rest rlt)))))))

(defun parse-lambda-list (lambda-list &rest args &key &allow-other-keys)
  ;; Return the parsed lambda list, a list of its variables, and a
  ;; list of anonymous variables.
  (let ((recognized (apply #'recognize-lambda-list lambda-list args)))
    (multiple-value-bind (ok problems) (validate-keywords recognized)
      (unless ok
        (scold "keyword troubles:~:{ ~S -> ~S~^,~}"
               (mapcar #'second problems))))
    (multiple-value-bind (anonymized anonymous) (anonymize-variables recognized)
      (multiple-value-bind (uniques duplicates) (unique-variables anonymized)
        (unless (null duplicates)
          (scold "duplicate variable~P ~{~S~^, ~} in ~S"
                 (length duplicates) duplicates lambda-list))
        (values (coalesce-keywords anonymized) uniques anonymous)))))
