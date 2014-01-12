-- Checks that the correct type is used checking the using clause of the group

{-# OPTIONS_GHC -XTransformListComp #-}

module ShouldFail where

data Unorderable = Gnorf | Pinky | Brain

foo = [ ()
      | x <- [Gnorf, Brain]
      , then group using take 5
      ]
