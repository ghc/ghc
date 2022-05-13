{-# LANGUAGE TypeFamilies #-}
module T21328 where

import GHC.Exts
import Type.Reflection

type family F x
type instance F x = x

type family G x
type instance G x = x

class C a where
  m :: G a

cast :: forall a. F a -> (C a => Int) -> Int
cast = withDict  @(C a) @(F a)
