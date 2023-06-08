{-# LANGUAGE PolyKinds #-}

module T17841 where

import Data.Kind

data Proxy a = Proxy

type Foo :: k -> Constraint
class Foo t where foo :: Proxy (a :: t)
