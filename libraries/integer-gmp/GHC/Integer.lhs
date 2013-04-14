\begin{code}
{-# LANGUAGE CPP, MagicHash, NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Integer
-- Copyright   :  (c) The University of Glasgow 1994-2008
-- License     :  see libraries/integer-gmp/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- The 'Integer' type.
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
    divModInteger, divInteger, modInteger,
    quotRemInteger, quotInteger, remInteger,
    encodeFloatInteger, floatFromInteger,
    encodeDoubleInteger, decodeDoubleInteger, doubleFromInteger,
    -- gcdInteger, lcmInteger,
    andInteger, orInteger, xorInteger, complementInteger,
    shiftLInteger, shiftRInteger, testBitInteger,
    hashInteger,
 ) where

import GHC.Integer.Type

default ()
\end{code}

