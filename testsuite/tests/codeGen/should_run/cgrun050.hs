-- !! Test strict, recursive newtypes
-- This test made a pre-5.02 fall over
-- Reason: the seq arising from the !F didn't see that
-- the represtation of F is a function. 

-- NB It's crucial to compile this test *without* -O
-- The $ then prevents the 'F' from seeing the '\x'
-- and hence makes the evaluation happen at runtime

module Main ( main ) where

newtype F = F (Int -> Val)	-- NB: F and Val are
data Val = VFn !F |  VInt !Int	-- mutually recursive

f :: Val -> Val
f (VFn (F f)) = f 4

main = print (f (VFn (F $ (\x -> VInt (x+3)))))

instance Show Val where
  show (VInt n) = show n


