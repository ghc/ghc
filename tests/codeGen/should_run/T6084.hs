{-# LANGUAGE MagicHash, BangPatterns #-}
module Main where

import GHC.Exts

{-# NOINLINE f #-}
f :: (Int# -> Float# -> Double# -> String) -> String
f g = g 3# 4.0# 6.9## ++ " World!"

{-# NOINLINE p #-}
p :: Int# -> Float# -> Double# -> String
p i j k = "Hello"

{-# NOINLINE q #-}
q :: Int# -> Int# -> Float# -> Double# -> String
q _ i j k = "Hello"

{-# NOINLINE r #-}
r :: Int# -> Float# -> Double# -> String
r i = let !(I# z) = length [I# 1# .. I# i] in \j k -> p z j k
  -- ghc won't eta-expand around the length, because it has unknown cost

main = do
  putStrLn (f p)    -- fast call
  putStrLn (f r)    -- slow call: function but wrong arity
  let g = last [q 1#]
  putStrLn (f g)    -- slow call: thunk

