module Main (main) where

import Lib (greeting)
import Utils (shout)

main :: IO ()
main = putStrLn (shout greeting)
