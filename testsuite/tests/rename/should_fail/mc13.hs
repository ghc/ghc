-- Test for transform list comp which should work for monad comp aswell:
--
-- Test trying to use a function bound in the list comprehension as the transform function

{-# OPTIONS_GHC -XRankNTypes -XMonadComprehensions -XTransformListComp #-}

module RnFail048 where

functions :: [forall a. [a] -> [a]]
functions = [take 4, take 5]

output = [() | f <- functions, then f]


