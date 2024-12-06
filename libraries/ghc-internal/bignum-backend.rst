GHC Bignum backend
==================

ghc-internal contains the implementation of the infinite precision integral
types ("big numbers/bignum") that were previously provided by ghc-bignum (and by
integer-gmp/integer-simple before that):
   
* BigNat: a positive natural represented as an array of Word# in memory
* Natural: a positive natural represented either by a Word# or by a BigNat
* Integer: a signed integer represented either by an Int# or in sign-magnitude
  representation where the magnitude is represented by a BigNat

Natural and Integer have each two representations:

* a small one: Word# or Int# respectively
* a large one: based on BigNat

The small representation is used when the number fits in it. We do this because
GHC is very good at optimizing codes which use Word#/Int# representations
(e.g. storing the number in registers instead of in memory).

Backends
--------

Several backends providing the implementation of some BigNat operations are
supported:

* GMP: based on the `GNU Multiple Precision Arithmetic library
  <https://gmplib.org/>`_ library (adapted from the legacy integer-gmp package)

* Native: a pure Haskell implementation written from scratch by Sylvain Henry.
  It replaces the previous pure Haskell implementation provided by the
  integer-simple package. The major difference is that it uses a much more
  efficient memory representation (integer-simple was based on Haskell lists)
  and that it allows a lot more code sharing between the different backends than
  was previously possible between integer-gmp and integer-simple.

* FFI: an implementation that relies on external FFI calls. This backend can be
  useful:

  * for alternative GHC backends that target non native platforms (JavaScript,
    JVM, etc.): the backend can dynamically match and rewrite the FFI calls in
    order to call the appropriate platform specific BigNum API.
    
  * to test new native backends: just tweak the ghc-bignum build to link with
    the native library providing the implementation of the FFI calls

  Note that the FFI backend module contains the description of the interface
  that needs to be implemented by every backend.

This package has been designed to make the implementation of new backends
relatively easy. Previously you had to implement the whole Integer/Natural
interface, to create a new package, etc. Now everything is well contained and
you only have to implement a small part of the BigNat interface. If you want to
try to implement a new backend, you don't have to implement the whole interface
upfront as you can always use the implementation provided by the Native backend
as a fall back.


Avoiding `patError`
-------------------

ghc-bignum used to be below the `base` package and `base` used to provide the
`patError` wired-in function. Hence if we use the natural set of definitions for
functions, e.g.:

    integerXor (IS x) y      = ...
    integerXor x      (IS y) = ...
    integerXor ...

then GHC would not be smart enough (especially when compiling with -O0)
to see that all the cases are handled, and will thus insert calls to
`base:Control.Exception.Base.patError`. But we are below `base` in the
package hierarchy, so this causes link failure!

We therefore help GHC out, by being more explicit about what all the
cases are:

    integerXor a b = case a of
       IS x -> case b of
                IS y -> ...
                IN y -> ...
       ...

This might not be required anymore now that ghc-bignum has been merged with
ghc-internal and that `patError` has been moved from `base` to ghc-internal.
