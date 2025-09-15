{-# LANGUAGE MagicHash, BangPatterns #-}
module Main where

import GHC.Exts

{-# NOINLINE f #-}
f :: (Int# -> Float# -> Double# -> Float# -> Double# -> String) -> String
f g = g 3# 4.0# 5.0## 6.0# 6.9## ++ " World!"

{-# NOINLINE p #-}
p :: Int# -> Float# -> Double# -> Float# -> Double# -> String
p i j k l m = "Hello"

{-# NOINLINE q #-}
q :: Int# -> Int# -> Float# -> Double# -> Float# -> Double# -> String
q _ i j k l m = "Hello " ++ show (F# l) ++ " " ++ show (D# m)

{-# NOINLINE r #-}
r :: Int# -> Float# -> Double# -> Float# -> Double# -> String
r i = let !(I# z) = length [I# 1# .. I# i] in \j k l m -> p z j k l m
  -- ghc won't eta-expand around the length, because it has unknown cost

main = do
  putStrLn (f p)    -- fast call
  putStrLn (f r)    -- slow call: function but wrong arity
  let g = last [q 1#]
  putStrLn (f g)    -- slow call: thunk
