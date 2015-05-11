{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -w #-}  -- Turn off deprecation for OverlappingInstances
-- | Safe, as we now check at overlap occurence, not defenition.
module UnsafeInfered08_A where

g :: Int
g = 1

