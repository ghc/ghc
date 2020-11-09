{-# LANGUAGE RankNTypes, PolyKinds, DataKinds, TypeFamilies #-}
module GHCBug where
import Data.Kind

data Proxy p = Proxy

data KProxy (a :: Type) = KProxy

h :: forall k r . (Proxy ('KProxy :: KProxy k) ~ Proxy ('KProxy :: KProxy Type) => r) -> r
h x = undefined
