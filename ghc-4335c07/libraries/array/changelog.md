# Changelog for [`array` package](http://hackage.haskell.org/package/array)

## 0.5.2.0  *Jul 2017*

  * Bundled with GHC 8.2.1
  * Overflow check in `unsafeNewArray` (#229)
  * Fix and simplify handling of `Bool` arrays
  * Export `unsafeFreezeIOUArray` from `Data.Array.IO.Internals`
  * Drop support for GHC versions prior to GHC 7.8

## 0.5.1.1  *Apr 2016*

  * Bundled with GHC 8.0.1
  * Use `@since` syntax in Haddock comments
  * Don't needlessly call `bounds` in `Data.Array.Base.elems` (#10014)

## 0.5.1.0  *Mar 2015*

  * Bundled with GHC 7.10.1
  * Add role annotations for GHC >= 7.8 (#9220)

## 0.5.0.0  *Nov 2013*

  * Update to Cabal 1.10 format
  * Remove NHC and Hugs specific code
  * Remove deprecated function exports `Data.Array.IO.castIOUArray`,
    `Data.Array.MArray.unsafeFreeze`, `Data.Array.MArray.unsafeThaw`,
    and `Data.Array.ST.castSTUArray`; These functions are still
    available from the `Data.Array.Unsafe` module.

## 0.4.0.1  *Sep 2012*

  * Bundled with GHC 7.6.1
  * Fix inline rule shadowing warnings

## 0.4.0.0  *Feb 2012*

  * Bundled with GHC 7.4.1
  * Add support for SafeHaskell
  * New `Data.Array.IO.Safe` module
  * New `Data.Array.MArray.safe` module
  * New `Data.Array.ST.safe` module
  * New `Data.Array.Storable.Internals` module
  * New `Data.Array.Storable.Safe` module
  * New `Data.Array.Unsafe` module
