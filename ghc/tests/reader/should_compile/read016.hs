-- !!! Checking that both import lists and 'hiding' lists might
-- !!! be empty.
module ShouldSucceed where

import List  ()
import List  hiding ()

x :: Int
x = 1

