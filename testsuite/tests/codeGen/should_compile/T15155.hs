-- The static indirection b must become an alias of a (#15155):
-- an .equiv directive in the NCG, an alias in LLVM.  Darwin symbols
-- carry a leading underscore.
--
-- ASM:  .equiv {{_?}}T15155.b_closure,{{_?}}T15155.a_closure
-- LLVM: @T15155_b_closure = alias i8, i8* @T15155_a_closure
module T15155 (a, B(..), b) where

import Debug.Trace

newtype A = A Int
newtype B = B A

{-# NOINLINE a #-}
a = trace "evaluating a" A 42000

b = B a
