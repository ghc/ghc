module Main where

import Data.IntSet as C
import SetOperations

main = benchmark fromList True [("union", C.union), ("difference", C.difference), ("intersection", C.intersection)]
