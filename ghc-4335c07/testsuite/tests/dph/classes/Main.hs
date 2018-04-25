module Main where

import Data.Array.Parallel.PArray (PArray, fromList)

import DefsVect

main 
  = let v      = fromList [1..10]
        w      = fromList [11..20]
  in
  print $ [ fiveEq 5
          , cmpArrs v w
          , isFives (fromList $ replicate 10 5)
          , isEqs v
          ]
