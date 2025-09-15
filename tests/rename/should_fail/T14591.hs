-- Checks that the ordering constraint on the groupWith function is respected
{-# OPTIONS_GHC -XMonadComprehensions -XTransformListComp #-}

module T14591 where

import GHC.Exts (groupWith)

data Unorderable = Gnorf | Pinky | Brain
foo = [ ()
      | Gnorf <- [Gnorf, Brain]
      , then group by x using groupWith
      ]
