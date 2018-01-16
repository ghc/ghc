module Main where

import C (x)
import GHC.Exts (inline)

main = print (inline x)
