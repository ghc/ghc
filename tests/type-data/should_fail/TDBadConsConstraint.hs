{-# LANGUAGE TypeData #-}
{-# LANGUAGE GADTs #-}
module TDConsConstraints where

import Data.Kind (Type)

type data Foo :: Type -> Type where
  MkFoo3 :: Show a          => Foo a
