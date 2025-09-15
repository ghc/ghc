{-# OPTIONS_GHC -Wno-x-partial #-}

module Main (main) where

import Data.List (unsnoc)

main :: IO ()
main = do
  print $ unsnoc ([] :: [Int])
  print $ unsnoc [1]
  print $ unsnoc [1, 2, 3]
  print $ fst <$> unsnoc [undefined :: Int]
  print $ head . fst <$> unsnoc (1 : 2 : undefined)
  print $ head . fst <$> unsnoc [1..]
