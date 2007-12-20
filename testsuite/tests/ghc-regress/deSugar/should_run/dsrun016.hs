-- Tests grouping WITH a using clause but WITHOUT a by clause

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

import List(inits)

main = putStrLn (show output)
  where
    output = [ x
             | y <- [1..3]
             , x <- "hello"
             , then group using inits ]
