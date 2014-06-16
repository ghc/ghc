-- Checks that the types of the old binder and the binder 
-- implicitly introduced by grouping are linked

{-# OPTIONS_GHC -XTransformListComp #-}

module ShouldFail where

foo = [ x + 1
      | x <- ["Hello", "World"]
      , then group using take 5
      ]