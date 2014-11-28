{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, ScopedTypeVariables, GADTs, RankNTypes #-}
module T9574 where

data KProxy (t :: *) = KProxy
data Proxy p

class Funct f where
    type Codomain f :: *

instance Funct ('KProxy :: KProxy o) where
    type Codomain 'KProxy = NatTr (Proxy :: o -> *)

data NatTr (c :: o -> *) where
    M :: (forall (a :: o). Proxy a) -> NatTr (c :: o -> *)

p :: forall (c :: o -> *). NatTr c
p = M t where
    M t = undefined :: Codomain ('KProxy :: KProxy o)
