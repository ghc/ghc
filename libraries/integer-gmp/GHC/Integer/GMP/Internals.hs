{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules provides access to the 'Integer' constructors and
-- exposes some highly optimized GMP-operations.
--
-- Note that since @integer-gmp@ does not depend on `base`, error
-- reporting via exceptions, 'error', or 'undefined' is not
-- available. Instead, the low-level functions will crash the runtime
-- if called with invalid arguments.
--
-- See also
-- <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer GHC Commentary: Libraries/Integer>.

module GHC.Integer.GMP.Internals
    ( -- * The 'Integer' type
      Integer(..)

      -- * Number theoretic functions
    , gcdInt
    , gcdInteger
    , gcdExtInteger
    , lcmInteger
    , nextPrimeInteger
    , testPrimeInteger

      -- * Exponentiation functions
    , powInteger
    , powModInteger
    , powModSecInteger
    , recipModInteger

    -- * Import/export functions
    , sizeInBaseInteger
    , importIntegerFromByteArray
    , importIntegerFromAddr
    , exportIntegerToMutableByteArray
    , exportIntegerToAddr
    ) where

import GHC.Integer.Type
