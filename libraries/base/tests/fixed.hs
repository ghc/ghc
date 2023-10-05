{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Data.Fixed

nums :: Fractional a => [a]
nums = [0,7,7.1,7.01,7.9,7.09,5 + 7,3.2 - 7.8,5.75 * (-2)]

main :: IO ()
main = do mapM_ putStrLn $ doit (nums :: [Micro])
          mapM_ putStrLn $ doit (nums :: [Pico])

doit :: HasResolution a => [Fixed a] -> [String]
doit xs = [ showFun (signFun x)
          | showFun <- [show, showFixed True]
          , signFun <- [id, negate]
          , x <- xs ]

