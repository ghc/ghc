# A compatibility layer for `base`
[![Hackage](https://img.shields.io/hackage/v/base-compat.svg)][Hackage: base-compat]
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/base-compat.svg)](http://packdeps.haskellers.com/reverse/base-compat)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-MIT-brightgreen.svg)][tl;dr Legal: MIT]

[Hackage: base-compat]:
  http://hackage.haskell.org/package/base-compat
  "base-compat package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: MIT]:
  https://tldrlegal.com/license/mit-license
  "MIT License"

## Scope

The scope of `base-compat` is to provide functions available in later versions
of base to a wider (older) range of compilers.

In addition, successful library proposals that have been accepted to be part of
upcoming versions of `base` are also included.  This package is not intended to
replace `base`, but to complement it.

Note that `base-compat` does not add any orphan instances.  There is a separate
package [`base-orphans`](https://github.com/haskell-compat/base-orphans) for
that.

In addition, `base-compat` only backports functions. In particular, we
purposefully do not backport data types or type classes introduced in newer
versions of `base`. For more info, see the
[Data types and type classes](#data-types-and-type-classes)
section.

`base-compat` is intentionally designed to have zero dependencies. As a
consequence, there are some modules that can only be backported up to certain
versions of `base`. If an even wider support window is desired in these
scenarios, there also exists a `base-compat-batteries` package which augments
`base-compat` with certain compatibility package dependencies. For more info,
see the [Dependencies](#dependencies) section.

## Basic usage

In your cabal file, you should have something like this:

```
  build-depends:      base              >= 4.3
                    , base-compat       >= 0.9.0
```

Then, lets say you want to use the `isRight` function introduced with
`base-4.7.0.0`.  Replace:

```
import Data.Either
```

with

```
import Data.Either.Compat
```

_Note (1)_: There is no need to import both unqualified.  The `.Compat` modules
re-exports the original module.

_Note (2)_: If a given module `.Compat` version is not defined, that either
means that:

* The module has not changed in recent base versions, thus no `.Compat` is
  needed.
* The module has changed, but the changes depend on newer versions of GHC, and
  thus are not portable.
* The module has changed, but those changes have not yet been merged in
  `base-compat`: patches are welcomed!

## Using `Prelude.Compat`

If you want to use `Prelude.Compat` (which provides all the
AMP/Traversable/Foldable changes from `base-4.8.0.0`), it's best to hide
`Prelude`, e.g.:

    import Prelude ()
    import Prelude.Compat

    main :: IO ()
    main = mapM_ print (Just 23)

Alternatively, you can use the `NoImplicitPrelude` language extension:

    {-# LANGUAGE NoImplicitPrelude #-}
    import Prelude.Compat

    main :: IO ()
    main = mapM_ print (Just 23)

Note that we use

    mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()

from `Data.Foldable` here, which is only exposed from `Prelude` since
`base-4.8.0.0`.

Using this approach allows you to write code that works seamlessly with all
versions of GHC that are supported by `base-compat`.

## What is covered
So far the following is covered.

### For compatibility with the latest released version of `base`

 * `Prelude.Compat` incorporates the AMP/Foldable/Traversable changes and
   exposes the same interface as `Prelude` from `base-4.9.0.0`
 * `System.IO.Error.catch` is not re-exported from `Prelude.Compat` for older
   versions of `base`
 * `Text.Read.Compat.readMaybe`
 * `Text.Read.Compat.readEither`
 * `Data.Monoid.Compat.<>`
 * Added `bitDefault`, `testBitDefault`, and `popCountDefault` to `Data.Bits.Compat`
 * Added `toIntegralSized` to `Data.Bits.Compat` (if using `base-4.7`)
 * Added `bool` function to `Data.Bool.Compat`
 * Added `isLeft`, `isRight`, `fromLeft`, and `fromRight` to `Data.Either.Compat`
 * Added `forkFinally` to `Control.Concurrent.Compat`
 * Added `withMVarMasked` function to `Control.Concurrent.MVar.Compat`
 * Added `(<$!>)` function to `Control.Monad.Compat`
 * Weakened `RealFloat` constraints on `realPart`, `imagPart`, `conjugate`, `mkPolar`,
   and `cis` in `Data.Complex.Compat`
 * Added more efficient `maximumBy`/`minimumBy` to `Data.Foldable.Compat`
 * Added `($>)` and `void` functions to `Data.Functor.Compat`
 * `(&)` function to `Data.Function.Compat`
 * `($>)` and `void` functions to `Data.Functor.Compat`
 * `modifyIORef'`, `atomicModifyIORef'` and `atomicWriteIORef` to `Data.IORef.Compat`
 * `dropWhileEnd`, `isSubsequenceOf`, `sortOn`, and `uncons` functions to `Data.List.Compat`
 * Correct versions of `nub`, `nubBy`, `union`, and `unionBy` to `Data.List.Compat`
 * `asProxyTypeOf` with a generalized type signature to `Data.Proxy.Compat`
 * `modifySTRef'` to `Data.STRef.Compat`
 * `String`, `lines`, `words`, `unlines`, and `unwords` to `Data.String.Compat`
 * `gcoerceWith` to `Data.Type.Coercion.Compat`
 * `makeVersion` function to `Data.Version.Compat`
 * `traceId`, `traceShowId`, `traceM`, and `traceShowM` functions to `Debug.Trace.Compat`
 * `byteSwap16`, `byteSwap32`, and `byteSwap64` to `Data.Word.Compat`
 * `plusForeignPtr` to `Foreign.ForeignPtr.Compat`
 * `calloc` and `callocBytes` functions to `Foreign.Marshal.Alloc.Compat`
 * `callocArray` and `callocArray0` functions to `Foreign.Marshal.Array.Compat`
 * `fillBytes` to `Foreign.Marshal.Utils.Compat`
 * Added `Data.List.Compat.scanl'`
 * `showFFloatAlt` and `showGFloatAlt` to `Numeric.Compat`
 * `lookupEnv`, `setEnv` and `unsetEnv` to `System.Environment.Compat`
 * `unsafeFixIO` and `unsafeDupablePerformIO` to `System.IO.Unsafe.IO`
 * `RuntimeRep`-polymorphic `($!)` to `Prelude.Compat`
 * `RuntimeRep`-polymorphic `throw` to `Control.Exception.Compat`
 * `isResourceVanishedError`, `resourceVanishedErrorType`, and
   `isResourceVanishedErrorType` to `System.IO.Error.Compat`
 * `singleton` to `Data.List.Compat` and `Data.List.NonEmpty.Compat`
 * `hGetContents'`, `getContents'`, and `readFile'` to `System.IO`

## What is not covered

### Data types and type classes
`base-compat` purposefully does not export any data types or type classes that
were introduced in more recent versions of `base`. The main reasoning for this
policy is that it is not some data types and type classes have had their APIs
change in different versions of `base`, which makes having a consistent
compatibility API for them practically impossible.

As an example, consider the `FiniteBits` type class. It was introduced in
[`base-4.7.0.0`](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Bits.html#t:FiniteBits)
with the following API:

```haskell
class Bits b => FiniteBits b where
    finiteBitSize :: b -> Int
```

However, in [`base-4.8.0.0`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Bits.html#t:FiniteBits),
`FiniteBits` gained additional functions:

```haskell
class Bits b => FiniteBits b where
    finiteBitSize :: b -> Int
    countLeadingZeros :: b -> Int   -- ^ @since 4.8.0.0
    countTrailingZeros :: b -> Int  -- ^ @since 4.8.0.0
```

This raises the question: how can `FiniteBits` be backported consistently
across all versions of `base`? One approach is to backport the API exposed in
`base-4.8.0.0` on versions prior to `4.7.0.0`.  The problem with this is that
`countLeadingZeros` and `countTrailingZeros` are not exposed in `base-4.7.0.0`,
so instances of `FiniteBits` would have to be declared like this:

```haskell
instance FiniteBits Foo where
    finiteBitSize = ...
#if MIN_VERSION_base(4,8,0) || !(MIN_VERSION_base(4,7,0))
    countLeadingZeros = ...
    countTrailingZeros = ...
#endif
```

Another approach is to backport the API from `base-4.7.0.0` and to declare
additional methods outside of the class:

```haskell
#if MIN_VERSION_base(4,7,0) && !(MIN_VERSION_base(4,8,0))
countLeadingZeros :: FiniteBits b => b -> Int
countLeadingZeros = {- default implementation #-}
#endif
```

The situation is only slightly better for classes which exist across all versions of `base`,
but have grown their API. For example, it's tempting to define

```haskell
#if !(MIN_VERSION_base(4,8,0))
displayException :: Exception e => e -> String
displayException = show
#endif
```

As with the previous approach, you won't be able to define new members of the type
class without CPP guards. In other words, the non-CPP approach would limit
uses to the lowest common denominator.

As neither approach is a very satisfactory solution, and to embrace
consistency, we do not pursue either approach. For similar reasons, we do not
backport data types.

### Dependencies

`base-compat` is designed to have zero dependencies (besides libraries that
ship with GHC itself). A consequence of this choice is that there are certain
modules that have a "limited" support window. An important example of this is
`Prelude.Compat`, which backports the `Semigroup` class to versions of `base`
older than 4.11 (when it was added to the `Prelude`). Because `Semigroup` was
not added to `base` until `base-4.9`, `base-compat` cannot backport it to
any earlier version of `base` than this.

If you would instead desire to be able to use a version of `Prelude.Compat`
that _does_ backport `Semigroup` to even older versions of `base`, even if it
means pulling in other dependencies, then you are in luck. There also exists
a `base-compat-batteries` package, which exposes a strict superset of the API
in `base-compat`. `base-compat-batteries` has all the same modules as
`base-compat`, but exposes more functionality on more versions of `base` by
reexporting things from compatibility libraries whenever necessary. (For
instance, `base-compat-batteries` exports the `Semigroup` class from the
`semigroups` library when built against versions of `base` older than 4.9.)

Because `base-compat` and `base-compat-batteries` have the same module names,
they are quite easy to switch out for one another in library projects, at the
expense of having clashing names if one tries to import them in GHCi. To
work around this issue, `base-compat` and `base-compat-batteries` also provide
copies of each module with the suffix `.Repl` (for `base-compat`) and
`.Repl.Batteries` (for `base-compat-batteries`) to give them globally unique
namespaces in the event one wants to import them into GHCi.

Here is a list of compatibility libraries that `base-compat-batteries` depends
on, paired with the things that each library backports:

* [`bifunctors`](http://hackage.haskell.org/package/bifunctors)
  for:
  * The [`Bifunctor`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Bifunctor.html#t:Bifunctor)
    type class, introduced in `base-4.8.0.0`
  * The [`Bifoldable`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Bifoldable.html#t:Bifoldable)
    and [`Bitraversable`](http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Bitraversable.html#t:Bitraversable)
    type classes, introduced in `base-4.10.0.0`
* [`contravariant`](http://hackage.haskell.org/package/contravariant)
  for the [`Contravariant`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Contravariant.html#t:Contravariant)
  type class, introduced in `base-4.12.0.0`.
* [`fail`](http://hackage.haskell.org/package/fail)
  for the [`MonadFail`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad-Fail.html#t:MonadFail)
  type class, introduced in `base-4.9.0.0`
* [`nats`](http://hackage.haskell.org/package/nats)
  for the [`Natural`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Numeric-Natural.html)
  data type, introduced in `base-4.8.0.0`
* [`semigroups`](http://hackage.haskell.org/package/semigroups)
  for the [`Semigroup`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Semigroup)
  typeclass and the
  [`NonEmpty`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List-NonEmpty.html#t:NonEmpty),
  [`Min`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Min),
  [`Max`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Max),
  [`First`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:First),
  [`Last`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Last),
  [`WrappedMonoid`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:WrappedMonoid),
  [`Option`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Option),
  and
  [`Arg`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Semigroup.html#t:Arg)
  data types, introduced in `base-4.9.0.0`
* [`tagged`](http://hackage.haskell.org/package/tagged)
  for the [`Proxy`](http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-Proxy.html#t:Proxy)
  data type, introduced in `base-4.7.0.0`
* [`transformers`](http://hackage.haskell.org/package/transformers)
  for:
  * The [`Identity`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Functor-Identity.html#t:Identity)
    data type, introduced in `base-4.8.0.0`
  * The [`MonadIO`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad-IO-Class.html#t:MonadIO)
    type class; and the
    [`Compose`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Compose.html#t:Compose),
    [`Product`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Product.html#t:Product),
    and
    [`Sum`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Sum.html#t:Sum)
    data types, introduced in `base-4.9.0.0`
* [`void`](http://hackage.haskell.org/package/void)
  for the [`Void`](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-Void.html#t:Void)
  data type, introduced in `base-4.8.0.0`

## Supported versions of GHC/`base`

 * `ghc-9.0.*`  / `base-4.15.*`
 * `ghc-8.10.*` / `base-4.14.*`
 * `ghc-8.8.*`  / `base-4.13.*`
 * `ghc-8.6.*`  / `base-4.12.*`
 * `ghc-8.4.*`  / `base-4.11.*`
 * `ghc-8.2.*`  / `base-4.10.*`
 * `ghc-8.0.*`  / `base-4.9.*`
 * `ghc-7.10.*` / `base-4.8.*`
 * `ghc-7.8.*`  / `base-4.7.*`
 * `ghc-7.6.*`  / `base-4.6.*`
 * `ghc-7.4.*`  / `base-4.5.*`
 * `ghc-7.2.*`  / `base-4.4.*`
 * `ghc-7.0.*`  / `base-4.3.*`

We also make an attempt to keep `base-compat` building with GHC HEAD, but due
to its volatility, it may not work at any given point in time. If it doesn't,
please report it!

Patches are welcome; add tests for new code!
