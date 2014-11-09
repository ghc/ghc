{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "MachDeps.h"

-- |
-- Module      :  GHC.Integer.GMP.Internals
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- This modules provides access to the 'Integer' constructors and
-- exposes some highly optimized GMP-operations.
--
-- Note that since @integer-gmp2@ does not depend on `base`, error
-- reporting via exceptions, 'error', or 'undefined' is not
-- available. Instead, the low-level functions will crash the runtime
-- if called with invalid arguments.
--
-- See also
-- <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Libraries/Integer GHC Commentary: Libraries/Integer>.

module GHC.Integer.GMP.Internals
    ( -- * The 'Integer' type
      Integer(..)
    , isValidInteger#

      -- ** Basic 'Integer' operations

    , module GHC.Integer

      -- ** Additional 'Integer' operations
    , bitInteger
    , popCountInteger
    , gcdInteger
    , lcmInteger
    , sqrInteger

      -- ** Additional conversion operations to 'Integer'
    , wordToNegInteger
    , bigNatToInteger
    , bigNatToNegInteger

      -- * The 'BigNat' type
    , BigNat(..)

    , GmpLimb, GmpLimb#
    , GmpSize, GmpSize#

      -- **

    , isValidBigNat#
    , sizeofBigNat#
    , zeroBigNat
    , oneBigNat
    , nullBigNat

      -- ** Conversions to/from 'BigNat'

    , byteArrayToBigNat#
    , wordToBigNat
    , wordToBigNat2
    , bigNatToWord
    , indexBigNat#

      -- ** 'BigNat' arithmetic operations
    , plusBigNat
    , plusBigNatWord
    , minusBigNat
    , minusBigNatWord
    , timesBigNat
    , timesBigNatWord
    , sqrBigNat

    , quotRemBigNat
    , quotRemBigNatWord
    , quotBigNatWord
    , quotBigNat
    , remBigNat
    , remBigNatWord

    , gcdBigNat
    , gcdBigNatWord

      -- ** 'BigNat' logic operations
    , shiftRBigNat
    , shiftLBigNat
    , testBitBigNat
    , andBigNat
    , xorBigNat
    , popCountBigNat
    , orBigNat
    , bitBigNat

      -- ** 'BigNat' comparision predicates
    , isZeroBigNat
    , isNullBigNat#

    , compareBigNatWord
    , compareBigNat
    , eqBigNatWord
    , eqBigNatWord#
    , eqBigNat
    , eqBigNat#
    , gtBigNatWord#

      -- * Miscellaneous GMP-provided operations
    , gcdInt

    ) where

import GHC.Integer.Type
import GHC.Integer

default ()
