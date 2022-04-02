{-# LANGUAGE TypeFamilies #-}
module T21328 where

import GHC.Exts
import Type.Reflection

type family Id x
type instance Id x = x

cast :: forall a. Id (TypeRep a) -> (Typeable a => Int) -> Int
cast = withDict @(TypeRep a) @(Typeable a)
