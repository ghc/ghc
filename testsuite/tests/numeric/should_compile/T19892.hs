{-# LANGUAGE MagicHash #-}

module T19892 where

import GHC.Exts
import GHC.Num.Integer
import GHC.Num.Natural

foo :: Word# -> Word#
foo x = integerToWord# (IS (word2Int# x))

bar :: Int# -> Int#
bar x = integerToInt# (IS x)

baz :: Word# -> Word#
baz x = naturalToWord# (NS x)
