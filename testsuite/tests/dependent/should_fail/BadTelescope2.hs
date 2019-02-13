{-# LANGUAGE DataKinds, PolyKinds, ExplicitForAll #-}

module BadTelescope2 where

import Data.Kind
import Data.Proxy

data SameKind :: k -> k -> *

foo :: forall a k (b :: k). SameKind a b
foo = undefined

bar :: forall a k (b :: k) (c :: Proxy b) (d :: Proxy a). Proxy c -> SameKind b d
bar = undefined
