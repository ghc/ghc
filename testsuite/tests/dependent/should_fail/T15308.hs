{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
module T15308 where

import Data.Kind

data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where
  MkFoo :: Foo a f

f :: Foo a f -> String
f = show
