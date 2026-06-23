-- See Note [Stable Core dump order] in GHC.Core.Ppr.
--
-- Companion to T27296 that pins the ordering of *anonymous* top-level floats.
-- Under -O the boxed Int constants in sel's branches are floated to top level
-- as separate CAFs, all of which the compiler names "lvl" with noSrcSpan (see
-- newLvlVar). Before -dstable-core-dump-order their dump order was the
-- unique-driven processing order; the flag's content-based tie-break (rhsKey)
-- now orders them by literal value -- here 1000..6000, despite the scrambled
-- source order. This dump is intentionally *untidied* (-ddump-float-out), the
-- only place the "lvl" collision is observable; tidied dumps like -ddump-simpl
-- already give the floats distinct names (lvl, lvl1, ...).
module T27296b (sel) where

{-# NOINLINE sel #-}
sel :: Int -> Int
sel 0 = 5000
sel 1 = 1000
sel 2 = 4000
sel 3 = 2000
sel 4 = 3000
sel _ = 6000
