{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}
module T8616 where

import Data.Proxy
import GHC.Exts

withSomeSing :: forall (kproxy :: k). Proxy kproxy
withSomeSing = undefined :: (Any :: k)
  -- The 'k' is bought into scope by the type signature
  -- This is a type error, but should not crash GHC

foo = (undefined :: Proxy (a :: k)) :: forall (a :: k). Proxy a
  -- Again, the 'k' is bought into scope by the type signature
  -- No type error though