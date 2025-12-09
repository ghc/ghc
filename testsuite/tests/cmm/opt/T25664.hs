{-# OPTIONS_GHC -O -fno-full-laziness #-}
{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.Int

mb8 :: Int8 -> Int8
{-# OPAQUE mb8 #-}
mb8 (I8# i) = I8# (i `quotInt8#` (noinline intToInt8# 128#))

mb16 :: Int16 -> Int16
{-# OPAQUE mb16 #-}
mb16 (I16# i) = I16# (i `quotInt16#` (noinline intToInt16# 32768#))

main :: IO ()
main = print (mb8 minBound) >> print (mb16 minBound)

