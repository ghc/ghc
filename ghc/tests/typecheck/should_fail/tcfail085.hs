-- !!! Check that not supplying bindings for strict fields
-- !!! is flagged as being incorrect.
module ShouldFail where

data F
 = F { x :: Int, y :: !Int }

z :: F
z = F { x = 2 }

