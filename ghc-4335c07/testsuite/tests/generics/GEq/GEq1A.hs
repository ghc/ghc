{-# LANGUAGE TypeOperators, DefaultSignatures,
             FlexibleContexts, FlexibleInstances, MagicHash #-}

module GEq1A where

import GHC.Exts
import GHC.Generics

class GEq' f where
  geq' :: f a -> f a -> Bool

instance GEq' U1 where
  geq' _ _ = True

instance (GEq c) => GEq' (K1 i c) where
  geq' (K1 a) (K1 b) = geq a b

-- No instances for P or Rec because geq is only applicable to types of kind *

instance (GEq' a) => GEq' (M1 i c a) where
  geq' (M1 a) (M1 b) = geq' a b

instance (GEq' a, GEq' b) => GEq' (a :+: b) where
  geq' (L1 a) (L1 b) = geq' a b
  geq' (R1 a) (R1 b) = geq' a b
  geq' _      _      = False

instance (GEq' a, GEq' b) => GEq' (a :*: b) where
  geq' (a1 :*: b1) (a2 :*: b2) = geq' a1 a2 && geq' b1 b2

-- Unboxed types
instance GEq' UAddr where
  geq' (UAddr a1) (UAddr a2)     = isTrue# (eqAddr# a1 a2)
instance GEq' UChar where
  geq' (UChar c1) (UChar c2)     = isTrue# (eqChar# c1 c2)
instance GEq' UDouble where
  geq' (UDouble d1) (UDouble d2) = isTrue# (d1 ==## d2)
instance GEq' UFloat where
  geq' (UFloat f1) (UFloat f2)   = isTrue# (eqFloat# f1 f2)
instance GEq' UInt where
  geq' (UInt i1) (UInt i2)       = isTrue# (i1 ==# i2)
instance GEq' UWord where
  geq' (UWord w1) (UWord w2)     = isTrue# (eqWord# w1 w2)

class GEq a where
  geq :: a -> a -> Bool
  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)

-- Base types instances (ad-hoc)
instance GEq Char   where geq = (==)
instance GEq Int    where geq = (==)
instance GEq Float  where geq = (==)

-- Generic instances
instance (GEq a) => GEq (Maybe a)
instance (GEq a) => GEq [a]
