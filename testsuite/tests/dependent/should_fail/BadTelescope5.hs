{-# LANGUAGE DataKinds, PolyKinds, ExplicitForAll #-}

module BadTelescope5 where

import Data.Kind
import Data.Proxy

data SameKind :: k -> k -> *

bar :: forall a k (b :: k) (c :: Proxy b) (d :: Proxy a). Proxy c -> SameKind b d
bar = undefined
