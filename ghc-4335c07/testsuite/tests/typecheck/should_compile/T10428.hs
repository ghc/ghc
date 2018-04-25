module T10428 where

import Data.Coerce
coerceNewtype :: (Coercible (o r) (n m' r)) => [o r] -> [n m' r]
coerceNewtype = coerce
