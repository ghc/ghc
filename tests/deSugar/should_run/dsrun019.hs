-- Test transform WITHOUT a by clause

{-# OPTIONS_GHC -XTransformListComp #-}

module Main where

main = putStrLn (show output)
  where
    output = [ x 
             | x <- [1..10]
             , then take 5 ]