-- Checks that the correct type is used checking the using clause of 
-- the group when a by clause is present

{-# OPTIONS_GHC -XMonadComprehensions -XTransformListComp #-}

module ShouldFail where

foo = [ length x
      | x <- [1..10]
      , then group by x using take 2
      ]
