{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

-- | Compatibility module for pre ghc-bignum code.
module GHC.Integer.Logarithms
   ( wordLog2#
   , integerLog2#
   , integerLogBase#
   )
where

import qualified GHC.Num.Primitives as N
import qualified GHC.Num.Integer    as N
import GHC.Num.Integer (Integer)
import GHC.Prim

wordLog2# :: Word# -> Int#
wordLog2# i = word2Int# (N.wordLog2# i)

integerLog2# :: Integer -> Int#
integerLog2# i = word2Int# (N.integerLog2# i)

integerLogBase# :: Integer -> Integer -> Int#
integerLogBase# x y = word2Int# (N.integerLogBase# x y)
