
-- Checks that the ordering constraint on the implicit groupWith is respected

{-# OPTIONS_GHC -XMonadComprehensions #-}

module ShouldFail where

data Unorderable = Gnorf | Pinky | Brain

foo = [ ()
      | x <- [Gnorf, Brain]
      , then group by x
      ]
