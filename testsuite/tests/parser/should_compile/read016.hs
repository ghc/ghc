-- !!! Checking that both import lists and 'hiding' lists might
-- !!! be empty.
module ShouldCompile where

import Data.Tuple  ()
import Data.Tuple  hiding ()

x :: Int
x = 1

