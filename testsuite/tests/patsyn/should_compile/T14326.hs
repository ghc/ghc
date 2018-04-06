{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module T14326 where

data E a b = L' a | R b
pattern L :: forall b a. a -> E a b
pattern L a = L' a
{-# COMPLETE L, R #-}

testMono :: E (E Int Int) Int -> Int
testMono x = case x of
  L (L _) -> 0
  L (R _) -> 1
  R _ -> 2

testPoly :: E (E a b) c -> Int
testPoly x = case x of
  L (L _) -> 0
  L (R _) -> 1
  R _ -> 2
