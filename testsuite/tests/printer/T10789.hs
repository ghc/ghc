{-# LANGUAGE TypeFamilies, PolyKinds, UndecidableInstances #-}

module T10789 where

import Data.Proxy

type family F (a :: k) :: k
type instance F a = G a

type family G a
type instance G a = a

foo :: Proxy (F Maybe) -> Proxy Maybe
foo = id
