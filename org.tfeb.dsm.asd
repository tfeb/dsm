;;;; ASDF sysdcl for dsm
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.dsm"
  :description "Destructuring match"
  :version "1.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/dsm"
  :depends-on ("org.tfeb.tools.require-module"
               "org.tfeb.hax.collecting"
               "org.tfeb.hax.iterate"
               "org.tfeb.hax.simple-loops"
               "org.tfeb.hax.utilities"
               "org.tfeb.hax.spam")
  :in-order-to ((test-op (load-op "org.tfeb.dsm/test")))
  :serial t
  :components
  ((:file "pkg")
   (:file "low")
   (:file "pll")
   (:file "cpll")
   (:file "dsm")))

(defsystem "org.tfeb.dsm/test"
  :description "Destructuring match tests"
  :version "1.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/dsm"
  :depends-on ("org.tfeb.dsm" "parachute")
  :pathname "test/"
  :serial t
  :components
  ((:file "pkg")
   (:file "preamble")
   (:file "test-pll")
   (:file "test-ll")
   (:file "test-dsm")
   (:file "test-bugs")
   (:file "test-all")))

(defsystem "org.tfeb.dsm/bench"
  :description "Destructuring match benchmarks"
  :version "1.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/dsm"
  :depends-on ("org.tfeb.dsm")
  :pathname "bench/"
  :components
  ((:file "bench")))
