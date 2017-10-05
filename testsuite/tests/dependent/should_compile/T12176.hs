{-# LANGUAGE RankNTypes, TypeInType, GADTs, TypeFamilies #-}

module T12176 where

import Data.Kind

data Proxy :: forall k. k -> Type where
  MkProxy :: forall k (a :: k). Proxy a

data X where
  MkX :: forall (k :: Type) (a :: k). Proxy a -> X

type Expr = (MkX :: forall (a :: Bool). Proxy a -> X)

type family Foo (x :: forall (a :: k). Proxy a -> X) where
  Foo (MkX :: forall (a :: k). Proxy a -> X) = (MkProxy :: Proxy k)

type Bug = Foo Expr  -- this failed with #12176
