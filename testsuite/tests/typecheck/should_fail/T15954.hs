{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module T15954 where

import Data.Kind

type A a = Int
type B1 (a :: Type -> Type) = forall x. x -> x
type C1 = B1 A

data NonShow
type B2 (a :: Type -> Type) = Show NonShow => Int
type C2 = B2 A
