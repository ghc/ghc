{-# LANGUAGE TemplateHaskell, ParallelListComp #-}

module Main where

list = [ (x,y) | x <- [1..10], x `mod` 2 == 0 | y <- [2,6..50] ]

list' = $( [| [ (x,y) | x <- [1..10], x `mod` 2 == 0 | y <- [2,6..50] ] |] )

main = do putStrLn (show list)
          putStrLn (show list')
          putStrLn $ show (list == list')