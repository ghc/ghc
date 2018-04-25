{-# LANGUAGE PolyKinds, TypeApplications, ScopedTypeVariables #-}

module Bug where

import Data.Proxy

class C a where
  foo :: Proxy a

bar :: forall a. C a => Proxy a
bar = foo @a
