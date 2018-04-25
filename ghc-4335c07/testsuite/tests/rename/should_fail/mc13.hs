-- Test for transform list comp which should work for monad comp as well:
--
-- Test trying to use a function bound in the list comprehension as the transform function

{-# OPTIONS_GHC -XRankNTypes -XMonadComprehensions -XTransformListComp #-}

module RnFail048 where

functions :: [[a] -> [a]]
functions = [take 4, take 5]

output = [() | f <- functions, then f]


