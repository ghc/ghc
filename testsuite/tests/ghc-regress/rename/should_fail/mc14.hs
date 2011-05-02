-- Test for transform list comp which should work for monad comp aswell:
--
-- Test trying to use a function bound in the list comprehension as the group function

{-# OPTIONS_GHC -XRank2Types -XMonadComprehensions -XTransformListComp #-}

module RnFail049 where

import Data.List(inits, tails)

functions :: [forall a. [a] -> [[a]]]
functions = [inits, tails]

output = [() | f <- functions, then group using f]


