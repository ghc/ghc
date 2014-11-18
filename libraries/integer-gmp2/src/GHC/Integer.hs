{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

#include "MachDeps.h"

-- |
-- Module      :  GHC.Integer.Type
-- Copyright   :  (c) Herbert Valerio Riedel 2014
-- License     :  BSD3
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
--
-- This module exposes the /portable/ 'Integer' API.  See
-- "GHC.Integer.GMP.Internals" for the @integer-gmp@-specific internal
-- representation of 'Integer' as well as optimized GMP-specific
-- operations.

module GHC.Integer (
    Integer,

    -- * Construct 'Integer's
    mkInteger, smallInteger, wordToInteger,
#if WORD_SIZE_IN_BITS < 64
    word64ToInteger, int64ToInteger,
#endif
    -- * Conversion to other integral types
    integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, integerToInt64,
#endif

    -- * Helpers for 'RealFloat' type-class operations
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,

    -- * Arithmetic operations
    plusInteger, minusInteger, timesInteger, negateInteger,
    absInteger, signumInteger,

    divModInteger, divInteger, modInteger,
    quotRemInteger, quotInteger, remInteger,

    -- * Comparison predicates
    eqInteger,  neqInteger,  leInteger,  gtInteger,  ltInteger,  geInteger,
    compareInteger,

    -- ** 'Int#'-boolean valued versions of comparision predicates
    --
    -- | These operations return @0#@ and @1#@ instead of 'False' and
    -- 'True' respectively.  See
    -- <https://ghc.haskell.org/trac/ghc/wiki/PrimBool PrimBool wiki-page>
    -- for more details
    eqInteger#, neqInteger#, leInteger#, gtInteger#, ltInteger#, geInteger#,


    -- * Bit-operations
    andInteger, orInteger, xorInteger,

    complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,

    -- * Hashing
    hashInteger,
    ) where

import GHC.Integer.Type

default ()
