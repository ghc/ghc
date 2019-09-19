{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
module GHC.Integer.GMP.Internals
    ( -- * The 'Integer' type
      pattern S#
    , pattern Jn#
    , pattern Jp#
    , isValidInteger#

      -- ** Basic 'Integer' operations

    , module GHC.Integer

      -- ** Additional 'Integer' operations
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

    ) where

import GHC.Integer
import GHC.Num.Integer
import GHC.Types
import GHC.Prim

{-# COMPLETE S#, Jp#, Jn# #-}

{-# DEPRECATED S# "Use IS constructor instead" #-}
pattern S# :: Int# -> Integer
pattern S# i = IS i

fromBN# :: BigNat -> ByteArray#
fromBN# (BN# x) = x

fromIP :: Integer -> (# () | BigNat #)
fromIP (IP x) = (# | BN# x #)
fromIP _      = (# () | #)

fromIN :: Integer -> (# () | BigNat #)
fromIN (IN x) = (# | BN# x #)
fromIN _      = (# () | #)

{-# DEPRECATED Jp# "Use IP constructor instead" #-}
pattern Jp# :: BigNat -> Integer
pattern Jp# i <- (fromIP -> (# | i #))
   where
      Jp# i = IP (fromBN# i)

{-# DEPRECATED Jn# "Use IN constructor instead" #-}
pattern Jn# :: BigNat -> Integer
pattern Jn# i <- (fromIN -> (# | i #))
   where
      Jn# i = IN (fromBN# i)

{-# DEPRECATED isValidInteger# "Use integerCheck# instead" #-}
isValidInteger# :: Integer -> Int#
isValidInteger# = integerCheck#

{-# DEPRECATED gcdInteger "Use integerGcd instead" #-}
gcdInteger :: Integer -> Integer -> Integer
gcdInteger = integerGcd

{-# DEPRECATED lcmInteger "Use integerLcm instead" #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger = integerLcm

{-# DEPRECATED sqrInteger "Use integerSqr instead" #-}
sqrInteger :: Integer -> Integer
sqrInteger = integerSqr

{-# DEPRECATED wordToNegInteger "Use integerFromWordNeg# instead" #-}
wordToNegInteger :: Word# -> Integer
wordToNegInteger = integerFromWordNeg#

{-# DEPRECATED bigNatToInteger "Use integerFromBigNat instead" #-}
bigNatToInteger :: BigNat -> Integer
bigNatToInteger (BN# i) = integerFromBigNat i

{-# DEPRECATED bigNatToNegInteger "Use integerFromBigNatNeg instead" #-}
bigNatToNegInteger :: BigNat -> Integer
bigNatToNegInteger (BN# i) = integerFromBigNatNeg i

{-# DEPRECATED BigNat "Use BigNat from ghc-bignum package instead" #-}
data BigNat = BN# ByteArray#

type GmpLimb = Word
type GmpLimb# = Word#
type GmpSize = Int
type GmpSize# = Int#
