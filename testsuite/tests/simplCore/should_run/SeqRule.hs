
-- This test checks that the magic treatment of RULES 
-- for 'seq' works right. 
--
-- See Note [User-defined RULES for seq] in GHC.Types.Id.Make for more details

module Main where

{-# NOINLINE f #-}
f x = not x

{-# RULES 
     "f/seq" forall n.  seq (f n) = const True
 #-}

main = print (seq (f True) False)
