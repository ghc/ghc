-- Checks that the correct type is used checking the using clause of the group

{-# OPTIONS_GHC -XMonadComprehensions -XTransformListComp #-}

module ShouldFail where
import GHC.Exts( the )

data Unorderable = Gnorf | Pinky | Brain

foo = [ length x
      | x <- [Gnorf, Brain]
      , then group using take 5
      ]
