{-# LANGUAGE StaticPointers #-}

module StaticPointers01 where

import GHC.StaticPtr

f0 :: StaticPtr (Int -> Int)
f0 = static g

f1 :: StaticPtr (Bool -> Bool -> Bool)
f1 = static (&&)

f2 :: StaticPtr (Bool -> Bool -> Bool)
f2 = static ((&&) . id)

g :: Int -> Int
g = id
