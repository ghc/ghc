
-- This test checks that the magic treatment of RULES 
-- for 'seq' works right. 
--
-- See Note [RULES for seq] in MkId for more details

module Main where

{-# NOINLINE f #-}
f x = not x

{-# RULES 
     "f/seq" forall n e.  seq (f n) e = True
 #-}

main = print (seq (f True) False)
