{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Main(main,complement) where

import GHC.Base
import GHC.Num

complement (I# x#) = I# (word2Int# (int2Word# (4294967295#) `xor#` int2Word# (-1#)))

main = print (complement (-1))
