{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fdefines-known-key-names #-}
    -- Defines lots of functions that have BuiltinRules

module GHC.Internal.Bignum.BigNat where

import GHC.Internal.Bignum.WordArray
import GHC.Internal.Bignum.Primitives
import GHC.Internal.Prim
import GHC.Internal.Types as Rebindable

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
