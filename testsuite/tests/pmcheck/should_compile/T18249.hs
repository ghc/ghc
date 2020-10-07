{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module T18249 where

import GHC.Exts

f :: Int# -> Int
-- redundant, not just inaccessible!
f !_ | False = 1
f _ = 2

newtype UVoid :: TYPE ('BoxedRep 'Unlifted) where
  UVoid :: UVoid -> UVoid

g :: UVoid -> Int
-- redundant in a weird way:
-- there's no way to actually write this function.
-- Inhabitation testing currently doesn't find that UVoid is empty,
-- but we should be able to detect the bang as redundant.
g !_ = 1

h :: (# (), () #) -> Int
-- redundant, not just inaccessible!
h (# _, _ #) | False = 1
h _ = 2

i :: Int -> Int
i !_      | False = 1
i (I# !_) | False = 2
i _               = 3

