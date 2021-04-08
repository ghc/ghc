{-# LANGUAGE TypeFamilies #-}
module T17772 where

import Data.Kind
import Data.Proxy

class C1 f where
  type T1 (x :: f a) :: Type

  sT1 :: forall a (x :: f a).
         Proxy x -> T1 x

class C2 a where
  type T2 a (x :: b) :: Type

  sT2 :: forall b (x :: b).
         Proxy a -> Proxy x -> T2 a x

class C3 a where
  type T3 a :: b -> Type

  sT3 :: forall b (x :: b).
         Proxy a -> Proxy x -> T3 a x

class C4 a where
  type T4 a :: forall b. b -> Type

  sT4 :: forall b (x :: b).
         Proxy a -> Proxy x -> T4 a x
