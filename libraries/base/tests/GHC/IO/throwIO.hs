module Main where

import GHC.IO
import ThrowTestLib

main = runThrowTest throwIO
