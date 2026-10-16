{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}

module T27963 where

-- This test is pretty delicate because it relies on a function
-- that has BOTH a rule AND an INLINE pragma
--
-- See #27963 for some discussion
--
-- Still, it should (just) work

import GHC.Exts

newtype N = MkN Int

f x = x
{-# INLINE f #-}
{-# RULES "f/42" forall x. f x = 42::Int #-}

-- The rule "f/42" should fire in both h1 and h2
h1,h2 :: N -> Int
h1 n = f (coerce n) + 0
h2 n = (coerce f) n + 0   -- #27963: the rule did not fire here
