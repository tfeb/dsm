# Installing `dsm`
## With [Quicklisp](https://www.quicklisp.org/beta/)
This covers the case where you are using Quicklisp, but `dsm` is not yet in it and/or its dependencies are out of date (both of these things are currently true).  The solution is just to clone the needed repos into QL's `local-projects` directory.  You need:

- [dsm](https://tfeb.github.io/dsm/) via `git clone https://github.com/tfeb/dsm.git`;
- [TFEB.ORG Lisp tools](https://tfeb.github.io/tfeb-lisp-tools/), at least version 8.0.0, via `git clone https://github.com/tfeb/tfeb-lisp-tools.git`;
- [TFEB.ORG Lisp hax](https://tfeb.github.io/tfeb-lisp-hax/), at least version 5.0.0, via `git clone  https://github.com/tfeb/tfeb-lisp-hax.git`.

Given this, `(ql:quickload "org.tfeb.dsm")` should just work.  The tests system is `"org.tfeb.dsm/test"`, and the benchmark system is `"org.tfeb.dsm/bench"`.

## Using ASDF without Quicklisp
I have never done this.  But there are ASDF system definitions for everything, so as long as ASDF knows how to find it all this should just work I think.

## Using [`require-module` / `needs`](https://tfeb.github.io/tfeb-lisp-tools/#requiring-modules-with-searching-require-module)
If everything is where `require-module` knows to look, then `(needs (:org.tfeb.dsm :compile t))` will just work.  This is how I usually load it.