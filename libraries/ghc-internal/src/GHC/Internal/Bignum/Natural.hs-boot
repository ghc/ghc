{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

{-# OPTIONS_GHC -fdefines-known-key-names #-}
    -- Defines lots of functions that have BuiltinRules

module GHC.Internal.Bignum.Natural where

import {-# SOURCE #-} GHC.Internal.Bignum.BigNat
import GHC.Internal.Bignum.Primitives
import GHC.Internal.Prim
import GHC.Internal.Types

data Natural
   = NS !Word#
   | NB !BigNat#

naturalToWord# :: Natural -> Word#
naturalFromWord# :: Word# -> Natural
naturalFromBigNat# :: BigNat# -> Natural
naturalToBigNat# :: Natural -> BigNat#

naturalZero :: Natural
naturalMul :: Natural -> Natural -> Natural
naturalRem :: Natural -> Natural -> Natural
naturalShiftR# :: Natural -> Word# -> Natural

naturalIsZero :: Natural -> Bool
naturalTestBit# :: Natural -> Word# -> Bool#
naturalEq# :: Natural -> Natural -> Bool#
