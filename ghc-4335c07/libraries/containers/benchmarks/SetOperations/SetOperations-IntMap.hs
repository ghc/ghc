module Main where

import Data.IntMap as C
import SetOperations

main = benchmark (\xs -> fromList [(x, x) | x <- xs]) True [("union", C.union), ("difference", C.difference), ("intersection", C.intersection)]
