
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) Ian Lynagh 2007-2012
-- License     :  BSD3
--
-- Maintainer  :  igloo@earth.li
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- A simple definition of the 'Integer' type.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Integer (
    Integer, mkInteger,
    smallInteger, wordToInteger, integerToWord, integerToInt,
#if WORD_SIZE_IN_BITS < 64
    integerToWord64, word64ToInteger,
    integerToInt64, int64ToInteger,
#endif
    plusInteger, minusInteger, timesInteger, negateInteger,
    eqInteger, neqInteger, absInteger, signumInteger,
    leInteger, gtInteger, ltInteger, geInteger, compareInteger,
    eqInteger#, neqInteger#,
    leInteger#, gtInteger#, ltInteger#, geInteger#,
    divInteger, modInteger,
    divModInteger, quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, decodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger, -- XXX
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,
    hashInteger,
 ) where

import GHC.Integer.Type

