module T26391 where

import Data.Semigroup (getSum, getProduct)

-- This record update is invalid (no constructor has both 'getSum' and 'getProduct').
--
-- This test makes sure that GHC can handle reporting a good error even when
-- the parent constructors (here, Sum and Product) are out of scope.
a = undefined { getSum = undefined, getProduct = undefined }
