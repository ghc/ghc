{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, TypeOperators #-}

module T7939 where

class Foo a where
   type Bar a b

type family F a
type instance F Int = Bool

type family G a where
  G Int = Bool

type family H a where
  H False = True

type family J a where
  J '[] = False
  J (h ': t) = True

type family K a where
  K '[] = Nothing
  K (h ': t) = Just h

type family L (a :: k) b :: k where
  L Int Int = Bool
  L Maybe Bool = IO