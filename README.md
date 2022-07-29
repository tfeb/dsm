# Destructuring match
Common Lisp doesn't have any pattern-matching facilities in the language.  A number have been written: [CLiki](https://www.cliki.net/pattern%20matching "CLiki pattern matching") has a list: Marco Antoniotti's [CL-UNIFICATION](https://gitlab.common-lisp.net/cl-unification/cl-unification "CL-UNIFICATION") is my favourite, as I used to be interested in unification grammars.  Many of these systems are quite general: they seek to be able to match very general objects to be extensible and to have very good performance.  This causes inevitable hair in their implementations, and also means that they often make doing something rather simple much harder than it needs to be.

The simple thing that should be easy is providing a generalised version of `destructuring-bind`or, equivalently[^1], macro argument lists.  That's what `dsm` does, and that's *all* it does: if you understand `destructuring-bind` and `case` you can pretty much stop reading now: `destructuring-match` is pretty much `case` except that the cases are lambda lists for `destructuring-bind`.

`dsm`'s whole purpose in life is to allow you to pattern match against *source code*: it does not, for instance, support matching against instances of general classes, because instances of general classes do not occur in source code.  It is a tool to make doing what Lisp does best easier: implementing programming languages built on Lisp.  *And that is all it does.*  Because this is all it is meant to do,`dsm` cares about correctness, but it does not care at all about performance: the performance of macroexpansion never matters[^2].

## An example
As an example, let's consider a macro where there are a few possible variations on the syntax:

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

This can be improved with, for instance, a system like my [simple pattern matcher, `spam`](https://tfeb.github.io/tfeb-lisp-hax/#simple-pattern-matching-spam "spam"):

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

The underlying problem here is that, before you can use `destructuring-bind` you need to wrap numerous guards around it, and this is especially true if you want to allow variations in syntax which don't match the same lambda list.  What would be nice is something that did what `destructuring-bind` does, but which could *also* try several possibilities.  Like this:

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

This is what `dsm` lets you do: it provides a macro, `destructuring-match`, which understands lambda lists similar to `destructuring-bind`s although slightly extended, except that it also matches against many possible lambda lists, and that matches can have 'guard clauses' which allow arbitrary additional tests before a match succeeds.

Again,`dsm` is not intended as a general-purpose pattern matcher: all it does is allow matching against many possible lambda lists, succeeding on the first match.  Guard clauses allow some additional tests before a match succeeds, but that's it.  The best way to understand `dsm` is that it's a *tool for writing macros*: it's not anything more general than that.  But as a tool for writing macros it can make your life a *lot* easier.  It would be relatively simple to implement, on top of `destructuring-match`, a pattern-matching macro language like Scheme's `syntax-rules`although without hygiene of course[^3].

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

The lambda lists understood by `destructuring-match` are[^4] the same as the lambda lists understood by `destructuring-bind`, extended in two ways:

1. a 'lambda list' which is a symbol binds the whole value of the expression, in the same way that `(lambda x ...)` does in Scheme;
2. any variable whose name is `_`, regardless of package, is a 'blank', and is turned into an anonymous variable which is ignored, with each occurrence of such a variable being distinct.

The guards specified by a guard clause may be repeated, so `(:when ... :unless ... :when ...)` is perfectly legal.  Guards are evaluated after variables are bound but before the match is committed.  If the guards fail the next clause is tried.

Clauses which begin`otherwise` and `t` are the same: they're the default case, and bind no variables.

## Conditions
In addition, `dsm` exposes three condition classes:

**`dsm-error`** is the condition type of all errors that `dsm` knowingly signals.

**`dsm-error/yours`** is the type of errors signalled by `dsm` which it considers to be your fault: things like botched lambda lists and so on.

**`dsm-error/mine`** is the type of errors signalled by `dsm` which it considers to be its fault.  Please report any of these.

Of course, other errors may occur which it has not foreseen: report these also.

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

An example of blank variables: this function will extract a list of keyword variable names from the various possible keyword argument specifications allowed by CL:

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

Without blank variables, something like this would need to be covered in explicit`ignore` declarations.

## Another example: `define-destructuring-macro`
`destructuring-match` was designed for writing macros, and it's easy to use it to write this[^5]:

```lisp
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
```

This can then be used, for instance, like this:

```lisp
(define-matching-macro with-scrot
 ((_ (s) &body forms)
  (:when (symbolp s))
  `(call-with-simple-scrot (lambda (,s) ,@forms)))
 ((_ (s v) &body forms)
  (:when (symbolp s))
  `(call-with-general-scrot (lambda (,s) ,@forms) (ensure-scrot ,v)))
 (badness
  (error "~S is a bad bad thing" badness)))
```

So now it's very easy to write macros which accept several argument patterns and which can also report syntax errors usefully.

## Notes on the implementation
### Lambda lists
`dsm` has to implement its own parsing and compilation of lambda lists.  It is intended to be compatible with `destructuring-bind` with the extension of 'lambda lists' which are a symbol.  However there are many corner cases, especially around keyword handling: I *think* it gets these right, but there may be bugs remaining: please let me know if you find any.

There is a fairly extensive test-suite included with `dem` which tests quite a lot of the corner cases, but it is not completely comprehensive, and I may also just not understand a lot of the corner cases of lambda list parsing.

### Structure sharing
When `dsm` binds a variable to an object which is part of the structure of the lambda list, that object will actually be part of that structure.  So, for instance

```lisp
(let ((l '(a b c)))
  (destructuring-match l
    ((&rest r)
     (eq l r))))
```

will be true.

### `&rest` lists may not be
A lambda list like `(a &rest b)` just peels off the first element of the thing it's matching for `a` and then puts everything else in `b`: that may not be a proper list or even a cons.  In particular

| this                | is the same as this |
| ------------------- | ------------------- |
| `(a b ... &rest c)` | `(a b ... . c)`     |
| `(&rest r)`         | `r`                 |

and so on.  Checking that `&rest` lists are in fact proper lists is expensive and probably not actually useful since it would make cyclic structures impossible to match, so `dsm` does not waste time doing so.

### Declarations
Declarations are 'raised' to where they belong by the compiler, so something like

```lisp
(destructuring-match x
  ((&key y)
   (:when (evenp y))
   (declare (type integer y))
   y))
```

Will do the right thing, and the guard clause will be within the scope of the declaration.

However, **no attempt is made to recognise the alternative form of type declarations**: `(declare (integer y))` is simply not recognised at all.  That's because it's essentially not possible to reliably recognise that declarations of the form `(<something> ...)` are in fact type declarations at all because CL has no 'is this a type specifier?' predicate.  So if you want to declare types, use the long form[^6].

Other declaration types which affect variable bindings, such as `ignore`, `dynamic-extent` and so on, are also raised.

### Dead code
`dsm` can generate code like this as a particular case (this often happens when using `&rest` lists in particular):

```lisp
(let ((x '()))
  (if (not (null x))
      (fail ...)
      ...))
```

This can cause SBCL at least to mutter about eliminating dead code: I decided that the additional complexity to deal with this special case wasn't worth it: probably any serious compiler will work it out and remove the test.

### The lambda list parser & compiler
`dsm` contains the seeds of what could be a general-purpose lambda list parser & compiler, which could, in theory, be taught how to parse & compile other sorts of lambda lists, including lambda lists not native to CL.  At present these are not well-separated from the code that recognizes and compiles `destructuring-bind`-style lambda lists, but they might one day be.

### Performance
Since `dsm` is intended for use in macros I made no real attempt to worry about performance.  There is a small set of rudimentary benchmarks which compare its performance with `destructuring-bind` for various cases: the results are obviously implementation-dependent, but generally it seems to be between about 1/2 and 1/10 the speed.  Given that it's portable code I'm happy with this[^7].

### Layers
`dsm` contains an 'implementation' layer which has its own package, and which may one day expose more of it with some documented interface.  Currently this interface is entirely internal to `dsm` and may change at any time.

## Other notes
'Lambda lists' which are symbols happened by mistake (there's what is essentially an error in the recognizer where it's looking for dotted lambda lists), but they are in fact so useful that I decided this was a feature, not a bug.

## Lost futures
`dsm` contains a lambda list parser and compiler which, in principle, are fairly general.  Cleaning up and exposing their interfaces was too exhausting when I was writing `dsm` but might happen in future.

What constitutes a blank variable is parameterized internally and could be made user-configurable.  On the other hand everybody elsr uses `_`, so I am not sure  any useful purpose would be served by doing so.

## Package, module, feature, dependencies
`dsm` lives in `org.tfeb.dsm` and provides `:org.tfeb.dsm`.  There is an ASDF system definition for both it and its tests.

`dsm` depends on a fair number of other things I have written: if you have a recent Quicklisp distribution then it *should* know about all of them.  At least, by the time `dsm` makes it into Quicklisp it should.  If not, you need at least version 5 of [my CL hax](https://tfeb.github.io/tfeb-lisp-hax/ "TFEB.ORG Lisp hax"), and at least version 8 of [my CL tools](https://tfeb.github.io/tfeb-lisp-tools/ "TFEB.ORG Lisp tools").

---

Destructuring match is copyright 2022 by Tim Bradshaw.  See `LICENSE` for the license.

[^1]:	Almost equivalently: neither `destructuring-bind` not `destructuring-match` support the `&environment` lambda list keyword.

[^2]:	Once, perhaps, it did, but that was most of a lifetime ago.

[^3]:	A toy version of a macro to define pattern-matching macros like this is included as an example.

[^4]:	Or should be!

[^5]:	Note that this is 12 lines, 6 of which is code to handle docstrings.

[^6]:	I think you should always do this, anyway.

[^7]:	Apocryphally it also outperforms some of those hairy pattern matchers which obsess about performance, although they obviously do a lot more than `dsm` does.