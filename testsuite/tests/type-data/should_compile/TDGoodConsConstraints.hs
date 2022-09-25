{-# LANGUAGE TypeData #-}
{-# LANGUAGE GADTs #-}
module TDGoodConsConstraints where

import Data.Kind (Type)
import Data.Type.Equality

type data Foo :: Type -> Type where
  MkFoo1 :: a ~ Int         => Foo a
  MkFoo2 :: a ~~ Int        => Foo a
