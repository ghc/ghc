{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Function
import Data.Kind
import GHC.Generics
import GHC.TypeLits

-- inlined generic-data imports:
from' :: Generic a => a -> Rep a ()
from' = from

geq :: (Generic a, Eq (Rep a ())) => a -> a -> Bool
geq = (==) `on` from'

gcompare :: (Generic a, Ord (Rep a ())) => a -> a -> Ordering
gcompare = compare `on` from'


-- test case:
data A (v :: Symbol -> Type -> Type) a b deriving (Generic,Generic1)

instance (Eq a , (forall w z . Eq z => Eq (v w z)) , Eq b) => Eq (A v a b) where
  {-# INLINE (==) #-}
  (==) = geq

instance (Ord a , (forall w z . Eq z => Eq (v w z)) , (forall w z . Ord z => Ord (v w z)) , Ord b) => Ord (A v a b) where
  {-# INLINE compare #-}
  compare = gcompare

main :: IO ()
main = pure ()
