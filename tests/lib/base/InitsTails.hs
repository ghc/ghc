module Main (main) where

import Data.List (inits, inits1, tails, tails1)

main :: IO ()
main = do
  print $ inits "abc"
  print $ inits ([] :: [Int])
  print $ take 5 $ inits [1..]
  print $ take 3 $ inits ([1, 2] ++ undefined)

  print $ inits1 "abc"
  print $ inits1 ([] :: [Int])
  print $ take 3 $ inits1 [1..]
  print $ take 2 $ inits1 ([1, 2] ++ undefined)

  print $ tails "abc"
  print $ tails ([] :: [Int])
  print $ drop 1 (tails [undefined, 1, 2])

  print $ tails1 "abc"
  print $ tails1 ([] :: [Int])
  print $ drop 1 (tails1 [undefined, 1, 2])
