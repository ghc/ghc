{-# LANGUAGE NoExperimentalDependentTypes #-}
module NativeOpenZipDemandControls (rightFirstPair, strictPair, strictResult) where

{-# NOINLINE [1] rightFirstPair #-}
rightFirstPair :: [a] -> [b] -> [(a, b)]
rightFirstPair _ [] = []
rightFirstPair [] _ = []
rightFirstPair (x : xs) (y : ys) = (x, y) : rightFirstPair xs ys

{-# NOINLINE [1] strictPair #-}
strictPair :: [a] -> [b] -> [(a, b)]
strictPair [] _ = []
strictPair _ [] = []
strictPair (x : xs) (y : ys) = x `seq` ((x, y) : strictPair xs ys)

{-# NOINLINE [1] strictResult #-}
strictResult :: (a -> b -> c) -> [a] -> [b] -> [c]
strictResult f = go
  where
    go [] _ = []
    go _ [] = []
    go (x : xs) (y : ys) = let hd = f x y in hd `seq` (hd : go xs ys)
