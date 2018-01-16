{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ParallelListComp #-}
-- On GHC 6.0 and earlier, this parallel list comprehension generated
-- an incorrect unused-binding warning.

module ShouldCompile where

t :: [(Char,Char)]
t = [ (a,b) | a <- "foo" | b <- "bar" ]
