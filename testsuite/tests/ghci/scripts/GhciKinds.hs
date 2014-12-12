{-# LANGUAGE TypeFamilies #-}
module GhciKinds where

type family F a :: *
type instance F [a] = a -> F a
type instance F Int = Bool

-- test ":kind!" in the presence of compatible overlap
type instance F (Maybe a) = Char
type instance F (Maybe Int) = Char
