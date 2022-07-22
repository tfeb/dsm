(in-package :cl-user)

(needs (:org.tfeb.dsm :compile t :use t))

(defmacro define-matching-macro (name &body clauses)
  (let ((<whole> (make-symbol "WHOLE"))
        (<junk> (make-symbol "JUNK")))
    (destructuring-match clauses
      ((doc . the-clauses)
       (:when (stringp doc))
       `(defmacro ,name (&whole ,<whole> &rest ,<junk>)
          ,doc
          (destructuring-match ,<whole> ,@the-clauses)))
      (the-clauses
       `(defmacro ,name (&whole ,<whole> &rest ,<junk>)
          (destructuring-match ,<whole> ,@the-clauses))))))

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
