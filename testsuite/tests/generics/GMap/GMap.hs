{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DefaultSignatures          #-}

module GMap (
  -- * Generic map class
    GMap(..)
  ) where


import GHC.Generics

--------------------------------------------------------------------------------
-- Generic map
--------------------------------------------------------------------------------

class GMap t where
  gmap :: (a -> b) -> t a -> t b
  default gmap :: (Generic1 t, GMap (Rep1 t)) => (a -> b) -> t a -> t b
  gmap f = to1 . gmap f . from1

instance GMap Par1 where gmap f (Par1 x) = Par1 $ f x
instance GMap f => GMap (Rec1 f) where gmap f (Rec1 x) = Rec1 $ gmap f x

instance GMap U1 where gmap _ U1 = U1

instance GMap (K1 i c) where gmap _ (K1 x) = K1 x

instance (GMap a) => GMap (M1 i d a) where gmap f (M1 x) = M1 $ gmap f x

instance (GMap a, GMap b) => GMap (a :+: b) where
  gmap f (L1 x) = L1 $ gmap f x
  gmap f (R1 x) = R1 $ gmap f x

instance (GMap a, GMap b) => GMap (a :*: b) where
  gmap f (x :*: y) = gmap f x :*: gmap f y

-- Base types instances
instance GMap []   where gmap = map
instance GMap Maybe    where gmap = fmap
instance GMap ((,) a)    where gmap f ~(x, y) = (x, f y)
