{-# LANGUAGE StaticPointers #-}

module StaticPtr where

import GHC.StaticPtr

topLevelStatic :: StaticPtr String
topLevelStatic = static "this is a top-level"

nestedStatic :: (StaticPtr String, Int)
nestedStatic = (s, 42)
  where
    s = static "nested static"
    {-# NOINLINE s #-}

s1 :: StaticPtr Int
s1 = static 3

s2 :: StaticPtr String
s2 = static "hello world"
