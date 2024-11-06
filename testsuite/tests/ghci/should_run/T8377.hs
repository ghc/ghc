{-# LANGUAGE MagicHash #-}

-- For last in main
{-# OPTIONS_GHC -Wno-x-partial #-}

import System.Mem
import GHC.Base

main = do
  let list = concatMap buildThunk [0..10000]
  length list `seq` performGC
  print $ last list

buildThunk :: Int -> [Int]
buildThunk (I# k) = [f k]

f :: Int# -> Int
f x = I# x
