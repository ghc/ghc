module A (f, y) where

import B.C (T(..))

f (T x) = x

y = T 42


