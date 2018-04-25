module Main where

main = do { let a = \x -> seq undefined (+1)
          ; print $ (a `seq` a [] `seq` id) [0] }
