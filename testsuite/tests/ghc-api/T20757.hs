module Main where

import GHC.SysTools.BaseDir

main :: IO ()
main = findToolDir "/" >>= print
