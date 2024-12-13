;;;; Loader for dsm
;;;

(in-package :asdf-user)

(load (merge-pathnames (make-pathname :name "org.tfeb.dsm"
                                      :type "asd")
                       *load-truename*))

(load-system "org.tfeb.dsm")
