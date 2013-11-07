{-# LANGUAGE NoImplicitPrelude #-}

-- | This modules provides access to the 'Integer' constructors and
-- exposes some highly optimized GMP-operations.
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
