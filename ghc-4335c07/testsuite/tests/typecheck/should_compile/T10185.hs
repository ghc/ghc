module T10185 where

import Data.Coerce
import Data.Proxy

foo :: (Coercible (a b) (c d), Coercible (c d) (e f)) => Proxy (c d) -> a b -> e f
foo _ = coerce
