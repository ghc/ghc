{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}
{-# Language DeriveGeneric #-}

module T8479 where

import GHC.Generics
import Data.Kind (Type)

class Blah (a :: Type -> Type) where
  type F a :: Type -> Type

data Foo (f :: Type -> Type) a = MkFoo ((F f) a) deriving Generic1
