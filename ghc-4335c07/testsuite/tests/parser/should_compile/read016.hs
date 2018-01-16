-- !!! Checking that both import lists and 'hiding' lists might
-- !!! be empty.
module ShouldCompile where

import Data.List  ()
import Data.List  hiding ()

x :: Int
x = 1

