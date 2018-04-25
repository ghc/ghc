# Changelog for [`deepseq` package](http://hackage.haskell.org/package/deepseq)

## 1.4.3.0 *Apr 2017*

  * Bundled with GHC 8.2.1

  * Drop support for GHC 7.0 & GHC 7.2

  * Changed strictness behavior of generic `NFData` instances for
    constructor-less data types. Before, a generic `rnf`
    implementation would always `error` on a data type with no
    constructors. Now, it will force the argument, so if the argument
    is a diverging computation, a generic `rnf` implementation will
    actually trigger the diverging computation.
    ([#19](https://github.com/haskell/deepseq/issues/19))

  * Add new `rwhnf` function defined as `rwhnf !_ = ()`
    ([#3](https://github.com/haskell/deepseq/issues/3))

  * Add `(<$!!>) :: (Monad m, NFData b) => (a -> b) -> m a -> m b`
    ([#13](https://github.com/haskell/deepseq/issues/13))

  * Add `NFData1` and `NFData2` type classes
    ([#8](https://github.com/haskell/deepseq/issues/8))

  * Add `NFData` instance for `Down` for `base` versions prior to
    `base-4.6.0` which didn't yet export it via `Data.Ord`
    ([#28](https://github.com/haskell/deepseq/pull/28))

  * Add `NFData` instance for `Foreign.C.Types.CBool`
    ([#33](https://github.com/haskell/deepseq/pull/33))

  * Add `NFData` instance for `Ordering`
    ([#25](https://github.com/haskell/deepseq/pull/25))

  * Add `NFData1` and `NFData` instances for `Data.Functor.{Compose,Sum,Product}`
    ([#30](https://github.com/haskell/deepseq/pull/30))

  * Add `NFData`, `NFData1`, and `NFData2` instances for `(:~:)` and `(:~~:)`
    from `Data.Type.Equality`
    ([#31](https://github.com/haskell/deepseq/issues/31))

## 1.4.2.0  *Apr 2016*

  * Bundled with GHC 8.0.1

  * New instances for types provided by `semigroups` prior to
    `base-4.9` (i.e. `NonEmpty`, `Min`, `Max`, `Arg`,
    `Semigroup.First`, `Semigroup.Last`, `WrappedMonoid`, and
    `Option`) ([#11](https://github.com/haskell/deepseq/issues/11))

  * New instances for `Ptr` and `FunPtr`
    ([#10](https://github.com/haskell/deepseq/pull/10))

  * New instances for `IORef`, `STRef`, and `MVar`
    ([#6](https://github.com/haskell/deepseq/issues/6))

  * New instance for `ExitCode`
    ([#4](https://github.com/haskell/deepseq/issues/4))

  * New instances for `CallStack` and `SrcLoc`

  * Make `NFData (Proxy a)` instance poly-kinded

## 1.4.1.2  *Aug 2015*

  * Avoid the broken combination of GHC-7.2 with `array>=0.4`
    ([#7](https://github.com/haskell/deepseq/pull/7))

## 1.4.1.1  *Mar 2015*

  * Bundled with GHC 7.10.1
  * Drop redundant `ghc-prim` dependency

## 1.4.1.0  *Mar 2015*

  * Drop redundant constraints from a few `NFData` instances (if
    possible for a given `base` version)

## 1.4.0.0  *Dec 2014*

  * Switch to Generics based `DefaultSignature` `rnf` method
    implementation (based on code from `deepseq-generics`)

    **Compatibility Note**: if you need the exact default-method
    semantics of `deepseq` prior to 1.4, replace occurences of

        instance NFData XYZ

    by

        instance NFData XYZ where rnf x = seq x ()

  * New `NFData` instances for `base` types:

     - `Control.Applicative.Const`
     - `Control.Applicative.ZipList`
     - `Control.Concurrent.ThreadId`
     - `Data.Functor.Identity.Identity`
     - `Data.Monoid.{Dual,First,Last,Any,All,Sum,Product}`
     - `Data.Ord.Down`
     - `Data.Proxy.Proxy`
     - `Data.Typeable.Internal.TyCon`
     - `Data.Typeable.Internal.TypeRep`
     - `Data.Unique.Unique`
     - `Data.Void.Void`
     - `GHC.Fingerprint.Type.Fingerprint`
     - `Numeric.Natural.Natural`
     - `System.Mem.StableName.StableName`
     - `Foreign.C.Types.C*`

## 1.3.0.2  *Nov 2013*

  * Bundled with GHC 7.8.1
  * Update package description to Cabal 1.10 format
  * Add support for GHC 7.8
  * Drop support for GHCs older than GHC 7.0.1
  * Add `/since: .../` annotations to Haddock comments
  * Add changelog

## 1.3.0.1  *Sep 2012*

  * No changes

## 1.3.0.0  *Feb 2012*

  * Add instances for `Fixed`, `a->b` and `Version`

## 1.2.0.1  *Sep 2011*

  * Disable SafeHaskell for GHC 7.2

## 1.2.0.0  *Sep 2011*

  * New function `force`
  * New operator `$!!`
  * Add SafeHaskell support
  * Dropped dependency on containers

## 1.1.0.2  *Nov 2010*

  * Improve Haddock documentation

## 1.1.0.1  *Oct 2010*

  * Enable support for containers-0.4.x

## 1.1.0.0  *Nov 2009*

  * Major rewrite

## 1.0.0.0  *Nov 2009*

  * Initial release
