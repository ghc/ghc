-- !!! Checking that more than imported module can share a local
-- !!! local alias.
module ShouldCompile where

import qualified Data.List  as X
import qualified Data.Maybe as X

x :: Ord a => [a] -> [a]
x = X.sort

y :: Maybe a -> Bool
y = X.isJust
