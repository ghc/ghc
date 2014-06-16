-- Checks that the correct type is used checking the using clause of the transform

{-# OPTIONS_GHC -XTransformListComp #-}

module ShouldFail where

import Data.List(inits)

z :: [Int]
z = [x | x <- [3, 2, 1], then inits]

