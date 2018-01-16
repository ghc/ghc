 {-# LANGUAGE DataKinds, PolyKinds, RankNTypes, GADTs #-}

module T7481 where

import Data.Proxy

data D a where
  D1 :: a -> D a
  D2 :: (a~Int) => D a
  D3 :: forall (a::k) b. Proxy a -> D b

data Foo :: D * -> *