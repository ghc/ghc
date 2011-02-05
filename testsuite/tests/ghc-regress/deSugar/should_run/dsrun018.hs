-- Test grouping with both a using and a by clause

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

import Data.List(groupBy)
import GHC.Exts(the)

groupRuns :: Eq b => (a -> b) -> [a] -> [[a]]
groupRuns f = groupBy (\x y -> f x == f y)

main = putStrLn (show output)
  where
    output = [ (the x, product y)
             | x <- ([1, 1, 1, 2, 2, 1, 3])
             , y <- [4..6]
             , then group by x using groupRuns ]