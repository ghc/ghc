{-# LANGUAGE TypeFamilies, ExplicitForAll #-}

module T15828 where

class C a where
  type T a b

instance C (Maybe a) where
  type forall a b. T (Maybe a) b = b
