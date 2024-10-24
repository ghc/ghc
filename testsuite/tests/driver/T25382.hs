module Main where

import Data.Complex

main = do
    x <- readLn :: IO (Complex Int)
    print $ realPart x
