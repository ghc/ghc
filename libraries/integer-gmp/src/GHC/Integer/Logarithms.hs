{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

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

{-# DEPRECATED wordLog2# "Use GHC.Num.Primitives.wordLog2# from ghc-bignum package instead" #-}
wordLog2# :: Word# -> Int#
wordLog2# i = word2Int# (N.wordLog2# i)

{-# DEPRECATED integerLog2# "Use GHC.Num.Integer.integerLog2# from ghc-bignum package instead" #-}
integerLog2# :: Integer -> Int#
integerLog2# i = word2Int# (N.integerLog2# i)

{-# DEPRECATED integerLogBase# "Use GHC.Num.Integer.integerLogBase# from ghc-bignum package instead" #-}
integerLogBase# :: Integer -> Integer -> Int#
integerLogBase# x y = word2Int# (N.integerLogBase# x y)
