{-# LANGUAGE UnboxedTuples, MagicHash #-}

module T19882a where

import GHC.Exts

f1 :: (# State# RealWorld, Int, Int #) -> Bool -> Int
f1 x True  = 1
f1 x False = f1 x True

