{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
module T16326_Fail6 where

import Data.Kind

data Foo :: Type -> Type where
  MkFoo :: forall a -> a -> Foo a
