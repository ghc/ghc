module Main where

import Data.Ord
import Data.Functor.Classes

-- Should print GT
main :: IO ()
main = print $ compare1 (Down 1) (Down 2)
