{-# OPTIONS -fglasgow-exts #-}

-- This program should fail type checking because 'r' is an unboxed tuple.
-- In 5.05.1 we instead got:
--   ghc-5.04.1: panic! (the `impossible' happened, GHC version 5.04.1):
--	  codeGen/CgRetConv.lhs:83: Non-exhaustive patterns in function
--	  dataReturnConvPrim
--
-- In 6.0, it compiles and crashes at runtime because it enters R1
--
-- Should fail in typechecking

module ShouldFail where

type T a = Int -> (# Int, Int #)

f :: T a -> T a
f t = \x -> case t x of r -> r
