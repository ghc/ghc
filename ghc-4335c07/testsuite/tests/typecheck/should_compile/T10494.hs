module App where

import Data.Coerce

foo :: Coercible (a b) (c d) => a b -> c d
foo = coerce
