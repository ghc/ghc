{-# LANGUAGE UnboxedSums, MagicHash #-}

module Lib (flip, getInt) where

import GHC.Exts
import Prelude (Int)

{-# NOINLINE flip #-}
flip :: (# Int | Int# #) -> (# Int# | Int #)
flip (# i | #) = (# | i #)
flip (# | i #) = (# i | #)

{-# NOINLINE getInt #-}
getInt :: (# Int# | Int #) -> Int
getInt (# i | #) = I# i
getInt (# | i #) = i
