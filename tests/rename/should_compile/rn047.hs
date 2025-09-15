{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# LANGUAGE ParallelListComp #-}
-- GHC 6.4 erroneously reported that the 
-- bindings for q and z were unused
--
-- Note the parallel list comprehension,
-- which was the cause of the trouble

module ShouldCompile where

t :: [Int]
t = [ q | y <- [1..10] 
        | z <- [30..40], let q = z*z]
