module T11208 where

import qualified Prelude as P

f n = n P.+ 1

g h (P.Just x) = P.Just (h x)
g _ P.Nothing  = P.Nothing
