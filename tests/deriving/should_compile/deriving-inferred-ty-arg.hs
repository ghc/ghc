{-# LANGUAGE DataKinds, TypeFamilies #-}
module DerivingInferredTyArg where

import Data.Functor.Const
import Data.Proxy

class C a where
  m :: forall {k} (b :: k). Const a b

class C2 a where
  m2 :: forall {k} {b :: k}. Const a b

class C3 a where
  m3 :: forall {k} (b :: k) {p :: Proxy b}. Const a p

data VisProxy k (a :: k) = VisProxy

class C4 a where
  m4 :: forall {k} (b :: k) {p :: VisProxy k b}. Const a p

type family Any :: k

class C5 a where
  m5 :: Proxy Any -> a

newtype T a = MkT a
  deriving (C, C2, C3, C4, C5)
