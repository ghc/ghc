-- !!! Check that using a label belonging to another constructor
-- !!! is flagged as being incorrect.
module ShouldFail where

data F
 = F { x :: Int }
 | G { y :: Int }

z :: F
z = F { y = 2 }

