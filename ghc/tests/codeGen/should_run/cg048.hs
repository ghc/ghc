
-- The new register allocator in 4.08 had a bug wherein
-- flow edges away from an insn which does a jump through
-- a switch table were not being added to the flow graph,
-- which causes computation of live ranges and thus register
-- assignment to be wrong in the alternatives and default.
-- This was fixed properly in the head branch (pre 4.09)
-- and avoided in 4.08.1 by disabling jump table generation
-- in the NCG -- it generates trees of ifs instead.

module Main ( main ) where

main = print (map f [1 .. 7])



{-# NOINLINE f #-}
f :: Int -> Bool
f 7 = False
f 1 = False
f 4 = False
f 6 = False
f 5 = False
f x = if x * 10 == 20 then True else False
