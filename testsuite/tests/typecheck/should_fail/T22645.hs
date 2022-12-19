module T22645 where

import Data.Coerce

type T :: (* -> *) -> * -> *
data T m a = MkT (m a)

p :: Coercible a b => T Maybe a -> T Maybe b
p = coerce
