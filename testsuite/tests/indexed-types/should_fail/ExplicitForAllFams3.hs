{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ExplicitForAllFams3 where

type family H a b where
  forall a.   H [a] (Maybe b) = Double

type family J a
type instance forall a.   J (a, b) = Bool

data family K a
data instance forall a.   K (a, b)    = K4 Bool

data family L a
newtype instance forall a.   L (a, b)    = L4 { unL4 :: Bool    }

class C a where
  type CT a b
  data CD a b
instance C Int where
  type forall a.   CT [a] (Maybe b) = Bool
  data forall a.   CD [a] (Maybe b) = CD4 Bool
