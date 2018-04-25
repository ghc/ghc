
-- Checks that the ordering constraint on the groupWith function is respected

{-# OPTIONS_GHC -XTransformListComp #-}

module ShouldFail where

import GHC.Exts (groupWith)

data Unorderable = Gnorf | Pinky | Brain

foo = [ ()
      | x <- [Gnorf, Brain]
      , then group by x using groupWith
      ]
