
module Main (main) where

import Data.List

main :: IO ()
main = do print (genericLength [1..10000000] :: Int)
          print (genericLength [1..10000000] :: Integer)
