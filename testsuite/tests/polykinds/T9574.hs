{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, RankNTypes #-}
module T9574 where

import Data.Kind (Type)

data KProxy (t :: Type) = KProxy
data Proxy p

class Funct f where
    type Codomain f :: Type

instance Funct ('KProxy :: KProxy o) where
    type Codomain 'KProxy = NatTr (Proxy :: o -> Type)

data NatTr (c :: o -> Type) where
    M :: (forall (a :: o). Proxy a) -> NatTr (c :: o -> Type)

p :: forall o (c :: o -> Type). NatTr c
p = M t where
    M t = undefined :: Codomain ('KProxy :: KProxy o)
