{-# LANGUAGE TypeOperators, DefaultSignatures, FlexibleInstances, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import GHC.Generics hiding (C, D)

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


class GEq a where 
  geq :: a -> a -> Bool
  default geq :: (Generic a, GEq' (Rep a)) => a -> a -> Bool
  geq x y = geq' (from x) (from y)


-- Base types instances (ad-hoc)
instance GEq Char   where geq = (==)
instance GEq Int    where geq = (==)
instance GEq Float  where geq = (==)
{-
-- Generic instances
instance (GEq a) => GEq (Maybe a)
instance (GEq a) => GEq [a]
-}

data C = C0 | C1
  deriving Generic

data D a = D0 | D1 { d11 :: a, d12 :: (D a) }
  deriving Generic

data (:**:) a b = a :**: b
  deriving Generic

-- Example values
c0 = C0
c1 = C1

d0 :: D Char
d0 = D0
d1 = D1 'p' D0

p1 :: Int :**: Char
p1 = 3 :**: 'p'

-- Generic instances
instance                   GEq C
instance (GEq a)        => GEq (D a)
instance (GEq a, GEq b) => GEq (a :**: b)

-- Tests
teq0 = geq c0 c1
teq1 = geq d0 d1
teq2 = geq d0 d0
teq3 = geq p1 p1

main = mapM_ print [teq0, teq1, teq2, teq3]
