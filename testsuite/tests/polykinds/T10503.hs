{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}
module GHCBug where

data Proxy p = Proxy

data KProxy (a :: *) = KProxy

h :: forall k r . (Proxy ('KProxy :: KProxy k) ~ Proxy ('KProxy :: KProxy *) => r) -> r
h = undefined
