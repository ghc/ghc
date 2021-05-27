{-# LANGUAGE UnboxedTuples, MagicHash #-}

module T19882b where

import GHC.Exts

f2 :: (# State# RealWorld, Int #) -> Bool -> Int
f2 x True  = 1
f2 x False = f2 x True
