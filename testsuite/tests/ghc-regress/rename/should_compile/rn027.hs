-- !!! Checking that an imported module may still have
-- !!! a local alias without having used 'qualified'.
module ShouldCompile where

import Data.List  as X
import Data.Maybe as X

x :: Ord a => [a] -> [a]
x = X.sort

y :: Maybe a -> Bool
y = X.isJust
