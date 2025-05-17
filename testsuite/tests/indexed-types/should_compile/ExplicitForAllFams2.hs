{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ExplicitForAllFams2 where

import Data.Kind (Type)

-- Even more tests

type family CF a b where
  forall x y.         CF [x] (Maybe y) = (x,y)
  forall (z :: Type). CF z   z         = Bool
  forall.             CF _   _         = ()

type family OF a
type instance forall a b. OF (Maybe a, Either a b) = Either [a] b

data family DF a
data instance forall a b. DF (Maybe a, Either a b) = DF a a b

data family NF a
newtype instance forall a b. NF (Maybe a, Either a b) = NF { unNF :: Either [a] b }

class Cl a where
  type AT a b
  data AD a b
instance forall a. Cl (Maybe a) where
  type forall b. AT (Maybe a) b = b
  data forall b. AD (Maybe a) b = AD b

-- Should produce warnings

type family N a where
  forall t (a :: Type). N (t a) = [a]
  forall a.             N a     = ()

type family N' a where
  N' (t (a :: Type)) = [a]
  N' a               = ()
