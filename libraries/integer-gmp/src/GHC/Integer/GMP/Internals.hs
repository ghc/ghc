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
      Integer (S#,Jn#,Jp#)
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
import GHC.Natural
import GHC.Num.Integer (Integer(..))
import qualified GHC.Num.Integer as I
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
isValidInteger# = I.integerCheck#

{-# DEPRECATED gcdInteger "Use integerGcd instead" #-}
gcdInteger :: Integer -> Integer -> Integer
gcdInteger = I.integerGcd

{-# DEPRECATED lcmInteger "Use integerLcm instead" #-}
lcmInteger :: Integer -> Integer -> Integer
lcmInteger = I.integerLcm

{-# DEPRECATED sqrInteger "Use integerSqr instead" #-}
sqrInteger :: Integer -> Integer
sqrInteger = I.integerSqr

{-# DEPRECATED wordToNegInteger "Use integerFromWordNeg# instead" #-}
wordToNegInteger :: Word# -> Integer
wordToNegInteger = I.integerFromWordNeg#

{-# DEPRECATED bigNatToInteger "Use integerFromBigNat instead" #-}
bigNatToInteger :: BigNat -> Integer
bigNatToInteger (BN# i) = I.integerFromBigNat i

{-# DEPRECATED bigNatToNegInteger "Use integerFromBigNatNeg instead" #-}
bigNatToNegInteger :: BigNat -> Integer
bigNatToNegInteger (BN# i) = I.integerFromBigNatNeg i

type GmpLimb = Word
type GmpLimb# = Word#
type GmpSize = Int
type GmpSize# = Int#
