{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -w #-}  -- Turn off deprecation for OverlappingInstances
-- | Safe, as we now check at overlap occurrence, not definition.
module UnsafeInfered08_A where

g :: Int
g = 1

