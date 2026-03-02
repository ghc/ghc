{-# LANGUAGE MagicHash #-}
module SignExtW32 (signExtW32) where

import GHC.Exts
import GHC.Int

signExtW32 :: Int32 -> Int64
signExtW32 (I32# x) = I64# (intToInt64# (int32ToInt# x))
