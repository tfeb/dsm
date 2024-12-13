;;;; DSM extensions
;;;

#+org.tfeb.tools.require-module
(progn
  (org.tfeb.tools.require-module:needs
   ("dsm" :compile t)))

(in-package :org.tfeb.dsm/extensions)

;;;; Literals
;;; We think this is the right way.
;;;

(defmacro literals (&rest designators)
  "Expands to a form which will test if the designated variables are literals

Each designator may be

- a variable name, which is a literal if it is bound to a symbol with
  its own name;
- a list (variable-name &key as test): the variable is a literal if it
  is bound to a symbol which is one of the list AS tested with TEST.
  AS and TEST are evaluated.  The default for AS is a list of one
  element, the variable's name.

The second case is useful, for instance, as (literals (in :test
'string=)), which will match any symbol whose name is the string
\"IN\""
  `(and ,@(mapcar (lambda (designator)
                    (destructuring-match designator
                      ((name &key (as `'(,name)) (test '(function eql)))
                       (:when (symbolp name))
                       `(and (symbolp ,name)
                             (member ,name ,as :test ,test)))
                      (name
                       (:when (symbolp name))
                       `(eql ,name ',name))
                      (otherwise
                       (scold "~S is not a literal designator" designator))))
                  designators)))
