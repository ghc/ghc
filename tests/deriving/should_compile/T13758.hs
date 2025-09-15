{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -Wno-noncanonical-monoid-instances #-}

module T13758 where

import Data.Coerce
import GHC.Generics
import Data.Semigroup

-----

class Monoid' f where
  mempty'  :: f x
  mappend' :: f x -> f x -> f x

instance Monoid' U1 where
  mempty' = U1
  mappend' U1 U1 = U1

instance Monoid a => Monoid' (K1 i a) where
  mempty' = K1 mempty
  mappend' (K1 x) (K1 y) = K1 (x `mappend` y)

instance Monoid' f => Monoid' (M1 i c f) where
  mempty' = M1 mempty'
  mappend' (M1 x) (M1 y) = M1 (x `mappend'` y)

instance (Monoid' f, Monoid' h) => Monoid' (f :*: h) where
  mempty' = mempty' :*: mempty'
  mappend' (x1 :*: y1) (x2 :*: y2) = mappend' x1 x2 :*: mappend' y1 y2

memptydefault :: (Generic a, Monoid' (Rep a)) => a
memptydefault = to mempty'

mappenddefault :: (Generic a, Monoid' (Rep a)) => a -> a -> a
mappenddefault x y = to (mappend' (from x) (from y))

-----

newtype GenericMonoid a = GenericMonoid a

instance (Generic a, Monoid' (Rep a)) => Semigroup (GenericMonoid a) where
  (<>) = coerce (mappenddefault :: a -> a -> a)

instance (Generic a, Monoid' (Rep a)) => Monoid (GenericMonoid a) where
  mempty  = coerce (memptydefault  :: a)
  mappend = coerce (mappenddefault :: a -> a -> a)

data Urls = Urls String String String
  deriving (Show, Generic)

newtype UrlsDeriv = UD (GenericMonoid Urls)
  deriving (Semigroup, Monoid)
