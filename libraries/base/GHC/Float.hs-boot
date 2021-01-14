{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module GHC.Float (integerToFloat#, integerToDouble#) where

import GHC.Num.Integer (Integer)
import GHC.Prim

integerToFloat# :: Integer -> Float#
integerToDouble# :: Integer -> Double#
