{-# OPTIONS_GHC -Wincomplete-patterns -fforce-recomp #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

module T18610 where

import GHC.Exts
import Data.Type.Equality

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

--
-- All the following bindings should not emit PMC warnings
--

-- | Clause 1 is not redundant, but has inaccessible RHS. The marker should
-- prevent a warning.
i :: () -> Int
i () | False, considerAccessible = 1
i _                              = 2

-- | Clause 1 is accessible with or without the marker. It has no
-- impact on checking the other equations.
j :: Bool -> Int
j x = case (x, x) of
  (True,  True)  | considerAccessible -> 1
  (False, False) -> 2

-- | The 'Refl' makes the second clause inaccessible (even a bang would do).
-- The marker prevents a warning. Unfortunately, it has no effect on
-- @-Winaccessible-code@.
k :: Int :~: Bool -> Bool -> Int
k _    False                      = 1
k Refl _     | considerAccessible = 2

-- | Compared to 'g', the marked inaccessible clause comes first. It has no
-- impact on checking the other equations.
l :: Bool -> Int
l x = case (x, x) of
  (True,  False) | considerAccessible -> 1 -- No warning!
  (True,  True)  -> 2
  (False, False) -> 3

-- | Warning that the second GRHS is redundant would be unsound here.
m :: Int -> Int
m x | False <- considerAccessible = 1
    | otherwise                   = 2 -- Not redundant!
