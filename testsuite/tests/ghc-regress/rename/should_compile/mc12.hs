{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MonadComprehensions #-}
-- Test for parallel list comp, which should work for monad comp aswell:
--
-- On GHC 6.0 and earlier, this parallel list comprehension generated
-- an incorrect unused-binding warning.

module ShouldCompile where

t :: [(Char,Char)]
t = [ (a,b) | a <- "foo" | b <- "bar" ]
