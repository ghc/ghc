{-# OPTIONS_GHC -w #-}  -- Turn off deprecation for OverlappingInstances
{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE OverlappingInstances #-}
-- | Safe, as we now check at overlap occurrence, not definition.
module UnsafeInfered08_A where

g :: Int
g = 1

