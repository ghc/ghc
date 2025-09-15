
{-# LANGUAGE MagicHash #-}

module Q where

import GHC.Exts
import GHC.Num.Integer

v :: Int
v = I# (integerToInt# (IS 3#))

w :: Word
w = W# (integerToWord# (integerFromWord# 3##))

