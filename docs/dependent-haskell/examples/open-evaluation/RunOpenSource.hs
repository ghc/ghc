module Main (main) where

import qualified OpenSource

main :: IO ()
main = print (OpenSource.x :: Int, OpenSource.y :: Int, OpenSource.z :: Int)
