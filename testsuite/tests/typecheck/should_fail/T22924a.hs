module T22924a where

import Data.Coerce

newtype R = MkR [R]

f :: a -> [R]
-- Should give a civilised error
f = coerce
