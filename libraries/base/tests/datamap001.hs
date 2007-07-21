
{-
In the 6.6 era this printed [(5,"x")]; should be [(3,"b"),(5,"a")]
-}

module Main (main) where

import Data.Map

main :: IO ()
main = do let m = fromList [(3,"b"),(5,"a")]
              f k a = Just "x"
              m' = updateAt f 1 m
          print m'
