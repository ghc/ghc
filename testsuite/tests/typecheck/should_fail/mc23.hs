-- Checks that using the "by" clause in a transform requires a function parameter

{-# OPTIONS_GHC -XMonadComprehensions -XTransformListComp #-}

module ShouldFail where

import Data.List(take)

z = [x | x <- [1..10], then take 5 by x ]

