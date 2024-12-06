{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module GHC.Internal.Bignum.Natural where

import {-# SOURCE #-} GHC.Internal.Bignum.BigNat
import GHC.Internal.Bignum.Primitives
import GHC.Prim
import GHC.Types

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
