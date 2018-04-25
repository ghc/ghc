{-# LANGUAGE StaticPointers   #-}

module StaticPointersFail01 where

import GHC.StaticPtr

f0 :: StaticPtr Int
f0 = static g

g :: Int -> Int
g = id
