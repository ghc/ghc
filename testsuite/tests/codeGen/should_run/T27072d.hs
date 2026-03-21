{-# LANGUAGE StaticPointers #-}
module T27072d where

import GHC.StaticPtr

f :: StaticPtr Int
f = static 1

g :: StaticPtr Int
g = static 2
