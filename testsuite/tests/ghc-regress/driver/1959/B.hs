module Main where

import C (x)
import GHC.Base (inline)

main = print (inline x)
