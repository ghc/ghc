module M (Array, (!!!), array, isJust, Map, lookup, insert, empty) where

import Arr (Array, (!!!), array)
import Base (Map, lookup, insert, empty)
import Prelude hiding (lookup)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
