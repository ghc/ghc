-- This test makes sure that the derivied instance for 
-- 	Eq A
-- "sees" the non-derived instance for 
--	Eq B
--
-- In a version of GHC 5.05, this didn't happen, because the
-- deriving mechanism looked through A's rep-type and found Int

module Main where

newtype B = MkB Int 
instance Eq B where
  (MkB 1) == (MkB 2) = True	-- Non-standard equality
  (MkB a) == (MkB b) = False
  
newtype A = MkA B deriving( Eq )

main = print (MkA (MkB 1) == MkA (MkB 2))
-- Should say "True", because of B's non-standard instance
