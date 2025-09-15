{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeFamilies #-}

module ExplicitForAllFams4 where

type family J a
type instance forall a b. J [a]    = Float
type instance forall b.   J _      = Maybe b

data family K a
data instance forall a b. K (a, Bool) = K5 Float
data instance forall b.   K _         = K6 (Maybe b)

data family L a
newtype instance forall a b. L (a, Bool) = L5 { unL5 :: Float   }
newtype instance forall b.   L _         = L6 { unL56:: Maybe b }

class C a where
  type CT a b
  data CD a b

instance C Int where
  type forall a b. CT [a] (a,a)     = Float
  data forall a b. CD [a] (a,a)     = CD5 Float

instance C Bool where
  type forall b.   CT _ _           = Maybe b
  data forall b.   CD _ _           = CD6 (Maybe b)

instance C Double where
  type forall b. CT _ _ = Bool
  data forall b. CD _ _ = CD7
