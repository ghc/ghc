-- This one killed GHC 6.4 because it bogusly attributed 
-- the CPR property to the construtor T
-- Result: a mkWWcpr crash
-- Needs -prof -auto-all to show it up

module ShouldCompile where

newtype T a = T { unT :: a }

f = unT

test cs = f $ case cs of
		   [] -> T []
		   (x:xs) -> T $ test cs
