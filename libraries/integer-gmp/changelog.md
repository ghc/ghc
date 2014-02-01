# Changelog for [`integer-gmp` package](http://hackage.haskell.org/package/integer-gmp)

## 0.5.1.0  *Feb 2014*

  * Bundled with GHC 7.8.1

  * Improved Haddock documentation

  * New [PrimBool](https://ghc.haskell.org/trac/ghc/wiki/PrimBool)
    versions of comparision predicates in `GHC.Integer`:

        eqInteger# :: Integer -> Integer -> Int#
        geInteger# :: Integer -> Integer -> Int#
        gtInteger# :: Integer -> Integer -> Int#
        leInteger# :: Integer -> Integer -> Int#
        ltInteger# :: Integer -> Integer -> Int#
        neqInteger# :: Integer -> Integer -> Int#

  * New `GHC.Integer.testBitInteger` primitive for use with `Data.Bits`

  * Reduce short-lived heap allocation and try to demote `J#` back
    to `S#` more aggressively.  See also
    [#8647](https://ghc.haskell.org/trac/ghc/ticket/8647)
    for more details.

  * New GMP-specific binary (de)serialization primitives added to
    `GHC.Integer.GMP.Internals`:

        importIntegerFromByteArray
        importIntegerFromAddr
        exportIntegerToAddr
        exportIntegerToMutableByteArray
        sizeInBaseInteger

  * New GMP-implemented number-theoretic operations added to
    `GHC.Integer.GMP.Internals`:

        gcdExtInteger
        nextPrimeInteger
        testPrimeInteger
        powInteger
        powModInteger
        powModSecInteger
        recipModInteger
