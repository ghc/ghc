module T22924b where

import Data.Coerce

newtype R = MkR [R]
newtype S = MkS [S]

f :: R -> S
-- Blows the typechecker reduction stack
f = coerce
