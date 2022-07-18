(in-package :cl-user)

(needs (:org.tfeb.dsm :compile t :use t))

(defmacro define-matching-macro (name &body clauses)
  (let ((<whole> (make-symbol "WHOLE"))
        (<junk> (make-symbol "JUNK")))
    (multiple-value-bind (doc the-clauses)
        (if (stringp (first clauses)) 
            (values (first clauses) (rest clauses))
          (values nil clauses))
      `(defmacro ,name (&whole ,<whole> &rest ,<junk>)
         ,@(if doc (list doc) '())
         (declare (ignore ,<junk>))
         (destructuring-match ,<whole>
           ,@(mapcar (lambda (clause)
                       ;; probably SPAM would be easier here
                       (destructuring-match clause
                         (((_ . more) (k . ks) . body)
                          (:when (symbolp _)
                           :when (keywordp k))
                          `((,_ ,@more)
                            (,k ,@ks)
                            (declare (,(if (string= (symbol-name _) "_")
                                           'ignore 'ignorable) ,_))
                            ,@body))
                         (((_ . more) . body)
                          (:when (symbolp _))
                          `((,_ ,@more)
                            (declare (,(if (string= (symbol-name _) "_")
                                           'ignore 'ignorable) ,_))
                            ,@body))
                         (otherwise
                          clause)))
                     the-clauses))))))

(define-matching-macro bind*
  ((_ () . forms)
   `(locally ,@forms))
  ((_ ((var &optional (val nil))) . forms)
   (:when (symbolp var))
   `((lambda (,var) ,@forms) ,val))
  ((_ (var) . forms)
   (:when (symbolp var))
   `((lambda (,var) ,@forms) nil))
  ((_ ((var &optional (val nil)) . more) . forms)
   (:when (symbolp var))
   `((lambda (,var) (bind* ,more ,@forms)) ,val))
  ((_ (var . more) . forms)
   (:when (symbolp var))
   `((lambda (,var) (bind* ,more ,@forms)) nil))
  (otherwise
   (error "what even is this?")))

