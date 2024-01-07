module Main where

import Data.List (sort)
import Data.Semigroup (Arg(..))

main :: IO ()
main = do
  -- correctness
  test @Int []
  test [0]
  test [8, 0, 2, 3, 6, 1, 5, 10, 4, 7, 9]

  -- stability
  test [Arg 1 0, Arg 0 0, Arg 0 1, Arg 1 1, Arg 0 2]
  test [Arg 0 0, Arg 0 1, Arg 0 2]

test :: (Ord a, Show a) => [a] -> IO ()
test = print . sort
