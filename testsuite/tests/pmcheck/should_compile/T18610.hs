{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}

module T18610 where

import GHC.Exts

f :: Bool -> Int
f x = case (x, x) of
  (True,  True)  -> 1
  (False, False) -> 2
  (True,  False) -> 3 -- Warning: redundant

g :: Bool -> Int
g x = case (x, x) of
  (True,  True)  -> 1
  (False, False) -> 2
  (True,  False) | considerAccessible -> 3 -- No warning!

h :: Bool -> Int
h x = case (x, x) of
  (True,  True)  -> 1
  (False, False) -> 2
  (True,  False) | considerAccessible, False <- x -> 3
  -- Warning: Not exhaustive. A non-severe leaking implementation detail of
  -- Note [considerAccessible]

