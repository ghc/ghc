{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module GHC.Num.Natural where

import {-# SOURCE #-} GHC.Num.BigNat
import GHC.Num.Primitives
import GHC.Prim
import GHC.Types

data Natural
   = NS !Word#
   | NB !BigNat

naturalToWord# :: Natural -> Word#
naturalFromWord# :: Word# -> Natural
naturalToBigNat :: Natural -> BigNat
naturalFromBigNat :: BigNat -> Natural
naturalMul :: Natural -> Natural -> Natural
naturalRem :: Natural -> Natural -> Natural
naturalIsZero :: Natural -> Bool
naturalShiftR# :: Natural -> Word# -> Natural
naturalTestBit# :: Natural -> Word# -> Bool#
