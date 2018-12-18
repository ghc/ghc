{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module VisibleDependentQuantificationFail6 where

import Data.Kind

data Foo :: Type -> Type where
  MkFoo :: forall a -> a -> Foo a
