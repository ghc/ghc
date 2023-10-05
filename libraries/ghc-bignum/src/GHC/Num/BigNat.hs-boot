{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Num.BigNat where

import GHC.Num.WordArray
import GHC.Num.Primitives
import GHC.Prim

type BigNat# = WordArray#
data BigNat = BN# { unBigNat :: BigNat# }

bigNatIsZero# :: BigNat# -> Bool#
bigNatSize# :: BigNat# -> Int#
bigNatSubUnsafe :: BigNat# -> BigNat# -> BigNat#
bigNatMulWord# :: BigNat# -> Word# -> BigNat#
bigNatRem :: BigNat# -> BigNat# -> BigNat#
bigNatRemWord# :: BigNat# -> Word# -> Word#
bigNatShiftR# :: BigNat# -> Word# -> BigNat#
bigNatShiftL# :: BigNat# -> Word# -> BigNat#
bigNatCtz# :: BigNat# -> Word#
bigNatCtzWord# :: BigNat# -> Word#
