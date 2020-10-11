module Main where

import Data.List (transpose, foldl')

thingy :: [[[Int]]]
thingy = [ [[1],[2]], [[1..10^7], [3]]]

thingy2 :: [[[Int]]]
thingy2 = [ [[1],[2]], [[3], [2..10^7+1]]]

main = do
  htr : ttr <- pure $ transpose thingy
  print $ foldl' (+) 0 . head . tail $ htr
  print ttr

  htr2 : ttr2 <- pure $ transpose thingy2
  print $ foldl' (+) 0 . head . tail . head $ ttr2
  print htr2

