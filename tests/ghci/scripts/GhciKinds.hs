{-# LANGUAGE TypeFamilies #-}
module GhciKinds where

import Data.Kind (Type)

type family F a :: Type
type instance F [a] = a -> F a
type instance F Int = Bool

-- test ":kind!" in the presence of compatible overlap
type instance F (Maybe a) = Char
type instance F (Maybe Int) = Char
