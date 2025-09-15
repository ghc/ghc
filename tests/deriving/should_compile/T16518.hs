{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module T16518 where

import Data.Coerce
import Data.Kind
import Data.Type.Equality

-----

class HTestEquality1 (f :: forall k. k -> Type) where
  hTestEquality1 :: forall k1 k2 (a :: k1) (b :: k2).
                    f a -> f b -> Maybe (a :~~: b)
newtype T1 :: (forall k. k -> Type) -> (forall k. k -> Type) where
  MkT1 :: forall (f :: forall k. k -> Type) k (a :: k). f a -> T1 f a

deriving instance forall (f :: forall k. k -> Type).
                  HTestEquality1 f => HTestEquality1 (T1 f)

-----

class HTestEquality2 (f :: forall k -> k -> Type) where
  hTestEquality2 :: forall k1 k2 (a :: k1) (b :: k2).
                    f k1 a -> f k2 b -> Maybe (a :~~: b)
newtype T2 :: (forall k -> k -> Type) -> (forall k -> k -> Type) where
  MkT2 :: forall (f :: forall k -> k -> Type) k (a :: k). f k a -> T2 f k a

deriving instance forall (f :: forall k -> k -> Type).
                  HTestEquality2 f => HTestEquality2 (T2 f)
