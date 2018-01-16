-- Tests transform WITH a by clause

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

import Data.List(takeWhile)

main = putStrLn (show output)
  where
    output = [ (x * 10) + y
             | x <- [1..4]
             , y <- [1..4]
             , then takeWhile by (x + y) < 4]