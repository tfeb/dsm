(in-package :cl-user)

(needs ((:org.tfeb.dsm
         :org.tfeb.hax.trace-macroexpand)
        :compile t :use t))

(trace-macroexpand)

(defmacro bind* (bindings &body forms)
  (destructuring-match bindings
    (()
     `(locally ,@forms))
    (((var &optional (val nil)))
     (:when (symbolp var))
     `((lambda (,var) ,@forms) ,val))
    ((var)
     (:when (symbolp var))
     `((lambda (,var) ,@forms) nil))
    (((var &optional (val nil)) . more)
     (:when (symbolp var))
     `((lambda (,var) (bind* ,more ,@forms)) ,val))
    ((var . more)
     (:when (symbolp var))
     `((lambda (,var) (bind* ,more ,@forms)) nil))
    (otherwise
     (error "what even is this?"))))

(trace-macro bind*)
