-- !!! Checking that more than imported module can share a local
-- !!! local alias.
module ShouldCompile where

import qualified List  as X
import qualified Maybe as X

x :: Ord a => [a] -> [a]
x = X.sort

y :: Maybe a -> Bool
y = X.isJust
