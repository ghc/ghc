{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Base

main :: IO ()
main = do
  -- Overflow.
  let (# w1, i1 #) = subWordC# 1## 3##
  print (W# w1, I# i1)

  -- No overflow.
  let (# w2, i2 #) = subWordC# 3## 1##
  print (W# w2, I# i2)
