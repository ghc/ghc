module T4903 where

import T4903a

{-# SPECIALIZE eq :: TreeF Tree -> Tree -> Bool #-}
-- The pragma is only problematic if it is in a separate module

f :: Bool
-- If we don't use eq, there is no problem  
f = eq Tree tree  
