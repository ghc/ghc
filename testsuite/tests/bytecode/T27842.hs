{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-worker-wrapper -fno-cpr-anal #-}
-- Regression test for #27842.
--
-- The basic rundown is this:
-- We produce one interface file, however we store information dependent on the
-- code generation method in the interface file.
--
-- We happen to pick the native code information to store in the interface file.
--
-- When we compile a module M this way, and then generate byte code for code
-- depending on M based on M.hi we will assume certain exported definitions are EPT
-- tagged, despite the interpreter doing no such thing.
--
-- This then led to crashes in the interpreter.
--
-- We test three kinds of imported definitions:
-- * mk uses fun, a function whose TagSig says its result is tagged.
-- * bar uses just_con, a value whose LFInfo says it is a constructor.
-- * baz uses gt_con, the same for a nullary constructor.
module Main where

import T27842A (fun, just_con, gt_con)
import T27842N (S(..), use_S, M(..), use_M, N(..), use_N)

mk :: Int -> S
mk x = case fun x of (# xs, _ #) -> S xs
{-# NOINLINE mk #-}

bar :: M
bar = M just_con
{-# NOINLINE bar #-}

baz :: N
baz = N gt_con
{-# NOINLINE baz #-}

main :: IO ()
main = do
  print (use_S (mk 42))
  print (use_M bar)
  print (use_N baz)
