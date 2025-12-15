module SAKS_018 where

import Data.Kind (Type)

type T :: forall k -> k -> Type
data T j (x :: j)
