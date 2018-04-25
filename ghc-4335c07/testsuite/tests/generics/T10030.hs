module Main where

import GHC.Generics

main = do
  putStrLn $ packageName $ from $ Just True
  putStrLn $ packageName $ from $ True
