
{-# LANGUAGE MagicHash #-}

module Q where

import GHC.Exts
import GHC.Integer

v :: Int
v = I# (integerToInt (smallInteger 3#))

