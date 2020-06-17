{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Num.BigNat where

import GHC.Num.WordArray
import GHC.Prim

type BigNat = WordArray#

bigNatSubUnsafe :: BigNat -> BigNat -> BigNat
bigNatMulWord# :: BigNat -> Word# -> BigNat
bigNatRem :: BigNat -> BigNat -> BigNat
bigNatRemWord# :: BigNat -> Word# -> Word#
bigNatShiftR# :: BigNat -> Word# -> BigNat
bigNatShiftL# :: BigNat -> Word# -> BigNat
bigNatCtz# :: BigNat -> Word#
bigNatCtzWord# :: BigNat -> Word#
