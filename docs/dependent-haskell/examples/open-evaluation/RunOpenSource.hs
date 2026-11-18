module Main (main) where

import qualified OpenSource

main :: IO ()
main = do
    print (OpenSource.x :: Int, OpenSource.y :: Int, OpenSource.z :: Int)
    print ( OpenSource.first False (42 :: Int)
          , OpenSource.repeated (OpenSource.second False (42 :: Int)) )
    print ( OpenSource.routedFirst False (42 :: Int)
          , OpenSource.routedSecond False (42 :: Int) )
