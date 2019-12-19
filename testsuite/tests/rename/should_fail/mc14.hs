-- Test for transform list comp which should work for monad comp as well:
--
-- Test trying to use a function bound in the list comprehension as the group function

{-# OPTIONS_GHC -XRankNTypes -XMonadComprehensions -XTransformListComp #-}

module RnFail049 where

import Data.List(inits, tails)

functions :: [[a] -> [[a]]]
functions = [inits, tails]

output = [() | f <- functions, then group using f]


