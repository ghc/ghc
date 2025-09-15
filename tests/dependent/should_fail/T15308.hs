{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module T15308 where

import Data.Kind

data Foo (a :: Type) :: forall b. (a -> b -> Type) -> Type where
  MkFoo :: Foo a f

f :: Foo a f -> String
f = show
