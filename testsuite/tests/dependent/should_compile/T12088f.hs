{-# LANGUAGE TypeFamilyDependencies, DataKinds #-}

module T12088f where

import Data.Kind
import Data.Proxy

type family F a = r | r -> a

type T :: F a -> Type
data T b where
  MkT :: T False

type instance F Int = Bool
type instance F Char = Proxy MkT
