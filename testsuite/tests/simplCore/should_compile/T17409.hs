module T17409 where

-- The bug was that id was inlined only after the
-- "gentle" simplifier pass, beucause CSE in GHC.Base
-- had commoned-up 'id' with 'breakpoint', and added
-- a NOINLINE[2] to the former.

-- The test just checks that id is inlined early.
f x = not (id x)
