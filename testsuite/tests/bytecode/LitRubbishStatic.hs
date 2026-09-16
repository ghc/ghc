-- A top-level constructor application whose boxed fields are all absent is
-- built with rubbish literals as its fields (Note [Absent fillers] in
-- GHC.Core.Opt.WorkWrap.Utils): every field of MkT is strict and lifted, and
-- 'useps' is OPAQUE and ignores its argument, so the worker for 'f' reboxes a
-- T out of nothing but fillers, and the result is floated to the top level.
--
-- The bytecode generator turns such a top-level binding into a static
-- constructor, and must give those fields a real heap pointer: the garbage
-- collector traces them. See Note [Rubbish literals of boxed type] in
-- GHC.StgToByteCode. Before that fix the fields were emitted as literal zero
-- words into an object whose info table says they are pointers, and the first
-- major GC to look at it followed a null.
module Main where

import System.Mem (performMajorGC)

data T = MkT ![Int] ![Int]

{-# OPAQUE useps #-}
useps :: T -> Int
useps _ = 0

f :: T -> Int
f ps = case ps of MkT _ _ -> 1 + useps ps
{-# NOINLINE f #-}

main :: IO ()
main = do
  -- Forces the CAF that holds the rubbish-filled T, so the collector is
  -- certain to reach it below.
  print (f (MkT [1,2] [3,4]))
  performMajorGC
  putStrLn "collected"
