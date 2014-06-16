{-# LANGUAGE MagicHash #-}

module T7796 where

import GHC.Prim

--
-- test for #7796
--
-- created by nicolas.frisby@gmail.com, feel free to email me!
--
-- a delicate interaction between specialisation and w/w creates a
-- binding that is dead but is allocated at run-time
--

--
-- we grep the -ddump-prep for $s$go, and the actual test expects a
-- particular number of hits
--
-- thus the test will fail in two scenarios:
--
--   * the actually interesting case where the zombie $s$go binding
--     survives, or
--
--   * the naming convention for specialised things changes, in which
--     case the Makefile rule for this test needs to be updated to
--     scrape the -ddump-prep output differently
--

--
-- the zombie binding is $sgo; here's how we reproduce it:
--
--   1. specialise go such that the RHS of $sgo uses both $sgo and go
--
--   2. worker-wrapper $sgo but *not* go
--
-- thus: $sgo uses $w$sgo uses go uses $sgo
--
-- the key point: the last "use" is only via a RULE; see the ticket
-- #7796 for more discussion and related tickets
--

data L = C Int# L | N Int# -- I'm using unboxed elements to avoid ww'd
                           -- unrelated to the bug

host :: Eq b => b -> L -> Bool
host b x =
  let go :: Eq a =>  -- must be used (to trigger specialise), but not
                     -- strict (else we ww the unspecialised version)

            a ->     -- must be strict, so that we ww the
                     -- specialisation

            L ->     -- not sure what this needs... but strict is
                     -- doing the trick

            Bool

      go d (N i) = d `seq` case i of
        0# -> True
        o  -> go b (N (i -# 1#)) -- NB must at (a ~ b)

      go d (C x xs) = (d == d) `seq` go d (C (x -# 1#) xs)
  in go (3 :: Int) x
