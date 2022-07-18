# Destructuring match
Common Lisp doesn't have any pattern-matching facilities in the language.  A number have been written: [CLiki](https://www.cliki.net/pattern%20matching "CLiki pattern matching") has a list: Marco Antoniotti's [CL-UNIFICATION](https://gitlab.common-lisp.net/cl-unification/cl-unification "CL-UNIFICATION") is my favourite, as I used to be interested in unification grammars.  Many of these systems are quite general: they seek to be able to match very general objects.  This causes inevitable hair in their implementations, and also means that they often make doing something rather simple much harder than it needs to be.

That simple thing, for me, is a generalised version of `destructuring-bind`or, equivalently, macro argument lists.  As an example, let's consider a macro where there are a few possible variations on the syntax:

```lisp
(with-foo x ... use x ...)
(with-foo (x a) ... use x ...)
(with-foo (x a b) ... use x ...)
(with-foo ((x y) a b) ...)
```

Well, to write a reasonably error-protected version of this you end up with code like, for instance

```lisp
(defmacro with-foo (binding &body forms)
  (typecase binding
    (symbol
     ...)
    (cons
     (destructuring-bind (thing . more) binding
       (typecase thing
         (symbol ...)
         (cons
          (unless (= (length thing) 2)
            (error ...))
          (destructuring-bind (var opt) thing
            ...))
         (t (error ...)))))
    (t (error ...))))
```

And this is just horrible.

This can be made better with, for instance, a system like my [simple pattern matcher, `spam`](https://tfeb.github.io/tfeb-lisp-hax/#simple-pattern-matching-spam "spam"):

```lisp
(defmacro with-foo (binding &body forms)
  (matching binding
    ((var)
     ...)
    ((one-of
      (list-matches (var) (any))
      (list-matches (var) (any) (any)))
     (destructuring-bind (var &optional a1 a2) binding
       ...))
    ((one-of
      (list-matches (list-matches (var) (any)) (any))
      (list-matches (list-matches (var) (any)) (any) (any)))
     (destructuring-bind ((var opt) &optional a1 a2) binding
       ...))
    (otherwise
     (error ...))))
```

This is a lot better.

The underlying problem here is that, before you can use `destructuring-bind` you need to wrap a bunch of guards around it, and this is especially true if you want to allow variations in syntax which don't match the same lambda list.  What would be nice is something that did what `destructuring-bind` does but *also* could try several possibilities.  Like this:

```lisp
(defmacro with-foo (binding &rest forms)
  (destructuring-match binding
    (var
     (:when (symbolp var))
     ...)
    ((var &optional a1 a2)
     (:when (symbolp var))
     ...)
    (((var opt) &optional a1 a2)
     (:when (symbolp var))
     ...)
    (otherwise
     (error ...))))
```

This is what `dsm` lets you do: it provides a macro, `destructuring-match`, which understands lambda lists similar to `destructuring-bind`s although slightly extended, except that it it also matches against many possible lambda lists, and that matches can have 'guard clauses' which allow arbitrary additional tests before a match succeeds.

`dsm` is not intended as a general-purpose pattern matcher: all it does is allow matching against many possible lambda lists, succeeding on the first match.  Guard clauses allow some additional tests before a match succeeds, but that's it.  The best way to understand `dsm` is that it's a *tool for writing macros*: it's not anything more general than that.  But as a tool for writing macros it can make your life a *lot* easier.  It would be relatively simple to implement, on top of `destructuring-match`, a pattern-matching macro language like Scheme's `syntax-rules`although without hygiene of course[^1].

## The interface
`dsm` provides a single macro: `destructuring-match`.

**`destructuring-match`** is a macro which combines something like `case` with a slightly extended`destructuring-bind`.

```lisp
(destructuring-match <expression>
  <clause> ...)
```

- `<expression>` is any expression.
- `<clause>` is one of
	- `(<lambda-list> [<guard>] ...)`;
	- `(otherwise ...)`;
	- `(t ...)`.
- For `<lambda-list>` see below.
- `<guard>` is a form like `(<when/unless) expression ...)`, where `<when/unless>` is `:when` or `:unless`.

The lambda lists understood by `destructuring-match` are[^2] the same as the lambda lists understood by `destructuring-bind`, extended in two ways:

1. a 'lambda list' which is a symbol binds the whole value of the expression, in the same way that `(lambda x ...)` does in Scheme;
2. any variable whose name is `_`, regardless of package, is a 'blank', and is turned into an anonymous variable which is ignored, with each occurrence of such a variable being distinct.

The guards specified by a guard clause may be repeated, so `(:when ... :unless ... :when ...)` is perfectly legal.  Guards are evaluated after variables are bound but before the match is committed.  If the guards fail the next clause is tried.

clauses which begin`otherwise` and `t` are the same: they're the default case.

## Conditions
In addition, `dsm` exposes three condition classes:

**`dsm-error`** is the condition type of all errors that `dsm` knowingly signals.

**`dsm-error/yours`** is the type of errors signalled by `dsm` which it considers to be your fault: things like botched lambda lists and so on.

**`dsm-error/mine`** is the type of errors signalled by `dsm` which it considers to be its fault.  Please report any of these.

Of course other errors may occur which it has not foreseen: report these too.

## Some simple examples
```lisp
(defmacro bind (v/v &body forms)
  (destructuring-match v/v
    (var
     (:when (symbolp var))
     `(let ((,var nil)) ,@forms))
    ((var val)
     (:when (symbolp var))
     `(let ((,var ,val)) ,@forms))
    (otherwise
     (error "bad binding ~S" v/v))))
```

is a trivial version of `let` which binds only one variable.

```lisp
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
```

is a more elaborate version of `let*`.

An example of a blank variables: this function will extract a list of keyword variable names from the various possible keyword argument specifications allowed by CL:

```lisp
(defun keyword-variable-names (keyword-argument-specifications)
  (mapcar (lambda (spec)
            (destructuring-match spec
              (v
               (:when (symbolp v))
               v)
              ((v _ &optional _)
               (:when (symbolp v))
               v)
              (((_ v) _ &optional _)
               (:when (symbolp v))
               v)
              (otherwise
               (error "not a keyword argument specification"))))
          keyword-argument-specifications))
```

without blank variables something like this would need to be covered in explicit`ignore` declarations.

## Notes on the implementation
### Lambda lists
`dsm` has to implement its own parsing and compilation of lambda lists.  It is intended to be compatible with `destructuring-bind` with the extension of 'lambda lists' which are a symbol.  However there are a lot of corner cases, especially around keyword handling: I *think* it gets these right but there may be bugs remaining: please let me know if you find any.

There is a fairly extensive test-suite included with `dem` which tests quite a lot of the corner cases, but it is not completely comprehensive and I may also just not understand a lot of the corner cases of lambda list parsing.

### Declarations
Declarations are 'raised' to where they belong by the compiler, so something like

```lisp
(destructuring-match x
  ((&key y)
   (:when (evenp y))
   (declare (type integer y))
   y))
```

Will do the right thing, and  the guard clause will be within the scope of the declaration.

However, **no attempt is made to recognise the alternative form of type declarations**: `(declare (integer y))` is simply not recognised at all.  That's because it's essentially not possible to reliably recognise that declarations of the form `(<something> ...)` are in fact type declarations at all because CL has no 'is this a type specifier?' predicate.  So if you want to declare types, use the long form[^3].

Other declaration types which affect variable bindings, such as `ignore`, `dynamic-extent` and so on, are also raised.

### The lambda list parser & compiler
`dsm` contains the seeds of what could be a general-purpose lambda list parser & compiler, which could, in theory, be taught how to parse & compile other sorts of lambda lists, including lambda lists not native to CL.  At present these are not well-separated from the code that recognizes and compiles `destructuring-bind`-style lambda lists, but they might one day be.

### Layers
`dsm` contains an 'implementation' layer which has its own package, and which may one day expose more of it with some documented interface.  Currently this interface is entirely internal to `dsm` and may change at any time.

## Other notes
I don't know whether `dsm`'s compiled lambda lists perform well or not: since it's intended for use in macros I made no real attempt to worry about performance.  Certainly it's unlikely that they perform as well as the system's implementation, but that was not the intention.

'Lambda lists' which are symbols happened by mistake (there's what is essentially an error in the recognizer where it's looking for dotted lambda lists), but it is in fact so useful that I decided it was a feature, not a bug.

## Lost futures
`dsm` contains a lambda list parser and compiler which, in principle, are fairly general.  Cleaning up and exposing their interfaces was too exhausting when I was writing `dsm` but might happen in future.

What constitutes a blank variable is parameterized internally and could be made user-configurable.  On the other hand everything uses `_`, so I am not sure  any useful purpose would be served by doing so.

## Package, module, feature, dependencies
`dsm` lives in `org.tfeb.dsm` and provides `:org.tfeb.dsm`.  There is an ASDF system definition for both it and its tests.

`dsm` depends on a fair number of other things I have written: if you have a recent Quicklisp distribution then it *should* know about all of them.  At least, by the time `dsm` makes it into Quicklisp it should.  If not, you need at least version 5 of [my CL hax](https://tfeb.github.io/tfeb-lisp-hax/ "TFEB.ORG Lisp hax"), and at least version 8 of [my CL tools](https://tfeb.github.io/tfeb-lisp-tools/ "TFEB.ORG Lisp tools").

---

Destructuring match is copyright 2022 by Tim Bradshaw.  See `LICENSE` for the license.

[^1]:	A toy version of a macro to define pattern-matching macros like this is included as an example.

[^2]:	Or should be!

[^3]:	I think you should always do this, anyway.